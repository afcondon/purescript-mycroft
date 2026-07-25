import { spawn } from "node:child_process";

export const spawnImpl = (cmd, args, onChunk, onFailure) => {
  const proc = spawn(cmd, args, { stdio: ["pipe", "pipe", "pipe"] });
  proc.stdout.setEncoding("utf8");
  proc.stderr.setEncoding("utf8");
  let stderr = "";
  proc.stdout.on("data", (chunk) => onChunk(chunk));
  proc.stderr.on("data", (chunk) => {
    stderr += chunk;
  });
  proc.on("error", (err) => onFailure(err.message));
  proc.on("exit", (code, signal) => {
    const detail = stderr.trim() === "" ? "" : `; stderr: ${stderr.trim()}`;
    onFailure(`exited (code ${code}, signal ${signal})${detail}`);
  });
  return proc;
};

export const writeImpl = (proc, s) => {
  try {
    proc.stdin.write(s);
  } catch (e) {
    // dying process: the exit handler reports the failure
  }
};

export const killImpl = (proc) => {
  try {
    proc.kill();
  } catch (e) {
    // already gone
  }
};
