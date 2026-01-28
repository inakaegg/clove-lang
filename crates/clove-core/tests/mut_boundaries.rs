use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;
use std::sync::Arc;

fn ctx() -> Arc<RuntimeCtx> {
    RuntimeCtx::new(EvalOptions::default(), &[])
}

#[test]
fn chan_put_rejects_mutable_collections() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(let [c (chan)] (chan-put! c (mut {:a 1})))")
        .expect_err("chan-put! should reject mutable collection");
    assert!(
        err.to_string().contains("imut"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn select_put_rejects_mutable_collections() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(select [[:put (chan) (mut {:a 1})]] {:default :ok})")
        .expect_err("select should reject mutable collection in put");
    assert!(
        err.to_string().contains("imut"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn agent_rejects_mutable_initial_state() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(agent (mut {:a 1}))")
        .expect_err("agent should reject mutable initial state");
    assert!(
        err.to_string().contains("imut"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn agent_send_rejects_mutable_payload() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(let [a (agent 0)] (agent-send! a (fn [s v] s) (mut {:a 1})))")
        .expect_err("agent-send! should reject mutable payload");
    assert!(
        err.to_string().contains("imut"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn promise_deliver_rejects_mutable_value() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(let [p (promise)] (promise-deliver! p (mut {:a 1})))")
        .expect_err("promise-deliver! should reject mutable value");
    assert!(
        err.to_string().contains("imut"),
        "unexpected error: {}",
        err
    );
}
