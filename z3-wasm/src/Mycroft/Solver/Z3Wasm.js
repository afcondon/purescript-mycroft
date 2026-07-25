import { init } from "z3-solver";

// One WASM module + worker pool per process, shared by all solvers.
let initPromise = null;
const ensureInit = () => {
  if (initPromise === null) initPromise = init();
  return initPromise;
};

export const startImpl = (onChunk) => (onFailure) => () =>
  ensureInit().then(({ Z3 }) => {
    const cfg = Z3.mk_config();
    const ctx = Z3.mk_context(cfg);
    Z3.del_config(cfg);
    let closed = false;
    // evals are serialised; lockstep means at most one is pending anyway
    let queue = Promise.resolve();
    return {
      send: (s) => () => {
        queue = queue.then(() =>
          Z3.eval_smtlib2_string(ctx, s).then(
            (out) => {
              if (!closed) onChunk(out.endsWith("\n") ? out : out + "\n")();
            },
            (err) => {
              if (!closed) onFailure(String(err))();
            }
          )
        );
      },
      close: () => {
        closed = true;
        queue = queue.then(() => {
          try {
            Z3.del_context(ctx);
          } catch (e) {
            // context already gone
          }
        });
      },
    };
  });

export const shutdownImpl = () => {
  if (initPromise === null) return Promise.resolve();
  return initPromise.then(({ em }) => {
    try {
      em.PThread.terminateAllThreads();
    } catch (e) {
      // workers already gone
    }
  });
};
