// Create a continuous legend
//
// This function is used to creaate a continuous legend and insert it
// into a parent HTML element (i.e. `div` container).
//
// @param el HTML element within which to insert the legend.
//
// @param x `Array` containing the following elements:
//   * `x.min_value`: `number` indicating the minimum value for the color bar
//                    within the legend.
//   * `x.max_value`: `number` indicating the maximum value for the color bar
//                    within the legend.
//   * `x.values`: `Array` of `number` objects indicating the labels for the
//                  color bar within the legend. These are also used to
//                  define the tick marks for the color bar too.
//   * `x.colors`: `Array` of `string` objects containing colors
//                 (e.g. `"#112233"`) to generate the gradient of colors for
//                 the color bar.
//
// @param units `string` containing the units for the legend (e.g. `"ha"`).
//
// @return `true` indicating success.
function continuousLegend(el, x, units) {
  // initialize lenged
  let l = document.createElement("div");
  l.className = "continuous-legend";

  // create color bar
  let colorbar = document.createElement("div");
  colorbar.className = "color-bar disable-if-inactive";

  // create container for ticks
  let ticks = document.createElement("div");
  ticks.className = "ticks";

  // create container for labels
  let labels = document.createElement("div");
  labels.className = "labels";

  for (let i = 0; i < x.values.length; ++i) {
    // create tick
    let tick = document.createElement("label");
    tick.className = "tick disable-if-inactive";
    tick.innerText = " ";
    ticks.appendChild(tick);

    // create label
    let label = document.createElement("label");
    label.className = "label disable-if-inactive";
    label.innerText = `${x.values[i]} ${x.units}`;
    labels.appendChild(label);
  }

  // construct legend
  l.appendChild(colorbar);
  l.appendChild(ticks);
  l.appendChild(labels);

  // add legend to parent container
  el.appendChild(l);
}
