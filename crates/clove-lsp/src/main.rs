use tower_lsp::LspService;
use tower_lsp::Server;

mod builtin_stubs;
mod docs;
mod document_store;
mod lsp_runtime;
mod server;

#[tokio::main]
async fn main() {
    eprintln!("clove-lsp: starting server");
    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let backend = server::Backend::new();
    let (service, socket) = LspService::new(|client| backend.with_client(client));
    Server::new(stdin, stdout, socket).serve(service).await;
    eprintln!("clove-lsp: server exited");
}
