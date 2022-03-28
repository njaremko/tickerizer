use std::collections::HashMap;

use radix_trie::Trie;
use radix_trie::TrieCommon;
use serde::Serialize;
use serde_json::Value;
use urlencoding::decode;
use worker::*;

mod tickers;
mod utils;

use tickers::TICKERS;

fn log_request(req: &Request) {
    console_log!(
        "{} - [{}], located at: {:?}, within: {}",
        Date::now().to_string(),
        req.path(),
        req.cf().coordinates().unwrap_or_default(),
        req.cf().region().unwrap_or_else(|| "unknown region".into())
    );
}

fn process_word(t: &Trie<String, String>, s: &str) -> String {
    let mut result = "".to_string();

    let first_match = t.get_ancestor(&s.to_uppercase());
    let first_ticker = first_match
        .map(|q| q.key().map(|t| t.as_ref()).unwrap_or(""))
        .unwrap_or_else(|| s);

    result += first_ticker;
    let remaining = &s[first_ticker.len()..];

    if remaining.len() + 3 < s.len() {
        let second_match = t.get_ancestor(&remaining[2..].to_uppercase());
        let second_ticker = second_match
            .map(|q| q.key().map(|t| t.as_ref()).unwrap_or(""))
            .unwrap_or_else(|| s);
        result += &remaining[0..2];
        result += second_ticker;
        result += &remaining[second_ticker.len()+2..];
    } else {
        result += &remaining;
    }

    result
}

fn process_words(trie: &Trie<String, String>, s: &str) -> String {
    s.split_whitespace()
        .map(|word| process_word(trie, word))
        .collect::<Vec<String>>()
        .join(" ")
}

#[derive(Serialize)]
struct SlackResponse {
    text: String,
    response_type: String,
}

fn parse_params(params: &str) -> HashMap<String, String> {
    params
        .split('&')
        .map(|kv| {
            let pair = kv
                .split('=')
                .take(2)
                .map(|t| t.to_string())
                .collect::<Vec<String>>();
            let key = pair.get(0).unwrap();
            let val = pair.get(1).unwrap();
            (key.to_owned(), val.to_owned())
        })
        .fold(HashMap::new(), |mut accum, (k, v)| {
            accum.insert(k, v);
            accum
        })
}

#[event(fetch)]
pub async fn main(req: Request, env: Env, _ctx: worker::Context) -> Result<Response> {
    log_request(&req);

    // Optionally, get more helpful error messages written to the console in the case of a panic.
    utils::set_panic_hook();

    // Optionally, use the Router to handle matching endpoints, use ":name" placeholders, or "*name"
    // catch-alls to match on specific patterns. Alternatively, use `Router::with_data(D)` to
    // provide arbitrary data that will be accessible in each route via the `ctx.data()` method.
    let router = Router::new();

    // Add as many routes as your Worker needs! Each route will get a `Request` for handling HTTP
    // functionality and a `RouteContext` which you can use to  and get route parameters and
    // Environment bindings like KV Stores, Durable Objects, Secrets, and Variables.
    router
        .get("/", |_, _| Response::ok("Hello from Workers!"))
        .get_async("/tickerize/:field", |_req, ctx| async move {
            if let Some(name) = ctx.param("field") {
                let mut trie = Trie::new();
                TICKERS.iter().for_each(|f| {
                    trie.insert(f.to_string(), f.to_string());
                });

                let r = decode(name).map_err(|_e| "")?.to_string();
                let resp = process_words(&trie, &r);
                Response::ok(resp)
            } else {
                Response::ok("Default Message")
            }
        })
        .post_async("/slack/tickerize", |mut req, _ctx| async move {
            let mut trie = Trie::new();
            TICKERS.iter().for_each(|f| {
                trie.insert(f.to_string(), f.to_string());
            });

            let form_data = req.form_data().await?;

            let text_param = match form_data.get("text") {
                Some(x) => match x {
                    FormEntry::Field(x) => x,
                    FormEntry::File(_) => "".into(),
                },
                None => "".into(),
            };

            let text = process_words(&trie, &text_param);

            Response::from_json(&SlackResponse {
                response_type: "in_channel".into(),
                text,
            })
        })
        .get("/slack", |mut _req, ctx| {
            let client_id = ctx.var("SLACK_CLIENT_ID")?;
            let mut redirect_url =
                "https://slack.com/oauth/v2/authorize?scope=commands&client_id=".to_string();
            redirect_url += &client_id.to_string();

            Response::redirect(Url::parse(&redirect_url)?)
        })
        .get_async("/slack/oauth", |req, ctx| async move {
            let client_id = ctx.var("SLACK_CLIENT_ID")?;
            let client_secret = ctx.secret("SLACK_CLIENT_SECRET")?;
            let query = req
                .url()?
                .query()
                .map(|c| c.to_owned())
                .unwrap_or_else(|| "".into());
            console_log!("This is the query we got: {query}");
            let params = parse_params(&query);
            let code = params
                .get("code")
                .map(|c| c.to_owned())
                .unwrap_or_else(|| "".into());
            console_log!("This is the code we got: {code}");
            let uri = "https://slack.com/api/oauth.v2.access?client_id=".to_string()
                + &client_id.to_string()
                + "&client_secret="
                + &client_secret.to_string()
                + "&code="
                + &code
                + "&grant_type=authorization_code";

            let r = Request::new(&uri, Method::Post)?;
            let mut resp = Fetch::Request(r).send().await?;

            let j = resp.json::<Value>().await?;
            Response::from_json(&j)
        })
        .get("/worker-version", |_, ctx| {
            let version = ctx.var("WORKERS_RS_VERSION")?.to_string();
            Response::ok(version)
        })
        .run(req, env)
        .await
}
