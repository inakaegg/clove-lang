import * as vscode from 'vscode';

const channel = vscode.window.createOutputChannel('Clove Selection Debug');
let revealed = false;

export function logSelectionDebug(message: string, reveal = false): void {
  const timestamp = new Date().toISOString();
  channel.appendLine(`[${timestamp}] ${message}`);
  if (reveal && !revealed) {
    channel.show(true);
    revealed = true;
  }
}

export function showSelectionDebugOutput(): void {
  channel.show(true);
  revealed = true;
}
