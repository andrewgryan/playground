import "./index.css"
import { createSignal } from "solid-js";

export default function Button() {
  const [count, setCount] = createSignal(0);
  return (
    <div class="p-2">
      <div>{count()}</div>
      <div class="flex space-x-2">
      <button
        onclick={() => setCount(() => count() - 1)}
        class="px-4 py-2 rounded bg-pink-400 shadow text-white hover:opacity-80"
      >
        Minus 1
      </button>
      <button
        onclick={() => setCount(() => count() + 1)}
        class="px-4 py-2 rounded bg-pink-400 shadow text-white hover:opacity-80"
      >
        Add 1
      </button>
    </div>
    </div>
  );
}
