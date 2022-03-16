use serde::Serialize;
use serde_json::Value;
use urlencoding::decode;
use utils::{process_words, parse_params, build_trie};
use worker::*;

mod tickers;
mod utils;

fn log_request(req: &Request) {
    console_log!(
        "{} - [{}], located at: {:?}, within: {}",
        Date::now().to_string(),
        req.path(),
        req.cf().coordinates().unwrap_or_default(),
        req.cf().region().unwrap_or_else(|| "unknown region".into())
    );
}

#[derive(Serialize)]
struct SlackResponse {
    text: String,
    response_type: String,
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
                let trie = build_trie();
                let r = decode(name).map_err(|_e| "")?.to_string();
                let resp = process_words(&trie, &r);
                Response::ok(resp)
            } else {
                Response::ok("Default Message")
            }
        })
        .post_async("/slack/tickerize", |mut req, _ctx| async move {
            let form_data = req.form_data().await?;
            let text_param = match form_data.get("text") {
                Some(x) => match x {
                    FormEntry::Field(x) => x,
                    FormEntry::File(_) => "".into(),
                },
                None => "".into(),
            };
            let trie = build_trie();
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
            let params = req
                .url()?
                .query()
                .map(|c| parse_params(c))
                .unwrap_or_default();
            let code = params
                .get("code")
                .map(|c| c.as_ref())
                .unwrap_or("");
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
