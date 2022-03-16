use std::collections::HashMap;

use cfg_if::cfg_if;
use radix_trie::{Trie, TrieCommon};

use crate::tickers::TICKERS;

cfg_if! {
    // https://github.com/rustwasm/console_error_panic_hook#readme
    if #[cfg(feature = "console_error_panic_hook")] {
        extern crate console_error_panic_hook;
        pub use self::console_error_panic_hook::set_once as set_panic_hook;
    } else {
        #[inline]
        pub fn set_panic_hook() {}
    }
}

pub fn build_trie() -> Trie<String, String> {
    let mut trie = Trie::new();
    TICKERS.iter().for_each(|f| {
        trie.insert(f.to_string(), f.to_string());
    });
    trie
}

fn process_word(t: &Trie<String, String>, s: &str) -> String {
    let mut result = "".to_string();

    let first_match = t.get_ancestor(&s.to_uppercase());
    let first_ticker = first_match
        .map(|q| q.key().map(|t| t.as_ref()).unwrap_or(""))
        .unwrap_or_else(|| s);

    result += first_ticker;
    let remaining = &s[first_ticker.len()..];

    if first_ticker.len() + 3 < s.len() {
        let second_match = t.get_ancestor(&remaining[2..].to_uppercase());
        let second_ticker = second_match
            .map(|q| q.key().map(|t| t.as_ref()).unwrap_or(""))
            .unwrap_or_else(|| s);
        result += &remaining[0..2];
        result += second_ticker;
        result += &remaining[second_ticker.len() + 2..];
    } else {
        result += &remaining;
    }

    result
}

pub fn process_words(trie: &Trie<String, String>, s: &str) -> String {
    s.split_whitespace()
        .map(|word| process_word(trie, word))
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn parse_params(params: &str) -> HashMap<String, String> {
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
