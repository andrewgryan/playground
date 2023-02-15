import "leaflet/dist/leaflet.css";
import * as L from "leaflet";
import { onCleanup, onMount, createEffect, createSignal } from "solid-js";

const Map = () => {
  let ref;
  const [map, setMap] = createSignal(null);
  const [layer, setLayer] = createSignal(openstreetmap());
  const [content, setContent] = createSignal(
    "<p>Hello world!<br />This is a nice popup.</p>"
  );

  // Let leaflet control the ref
  onMount(() => {
    setMap(L.map(ref).setView([51.505, -0.09], 13));
  });

  // Wire up object-oriented interface
  createEffect(() => {
    // local variables used to cache reactive context value
    const _map = map();
    const _layer = layer();
    if (_map != null) {
      _layer.addTo(_map);
      onCleanup(() => {
        _map.removeLayer(_layer);
      });
    }
  });

  createEffect(() => {
    const _map = map();
    if (_map != null) {
      let popup = L.popup()
        .setLatLng([51.505, -0.09])
        .setContent(content())
        .openOn(_map);
      console.log("Add", popup);
      console.log(_map._layers);
      onCleanup(() => {
        console.log("Cleanup", popup);
        // _map.removeLayer(popup);
      });
    }
  });

  // Simulate user clicks
  createEffect(() => {
    setTimeout(() => {
      setLayer(cartocdn());
      setContent("G'day mate!");
    }, 3000);
  });

  return <div ref={ref} style={{ height: "100vh" }} />;
};

const openstreetmap = () =>
  L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution:
      '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  });

const cartocdn = () =>
  L.tileLayer("https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png", {
    attribution:
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
    subdomains: "abcd",
    maxZoom: 20,
  });

function App() {
  return <Map />;
}

export default App;
