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
//                  Note that the minimum and maximum values in this array
//                  will always be bounded (inclusive) within the `x.min_value`
//                  and `x.max_value` elements.
//   * `x.colors`: `Array` of `string` objects containing colors
//                 (e.g. `"#112233"`) to generate the gradient of colors for
//                 the color bar. Note that the number of colors in this
//                 array will (in most cases) not be equal to the number
//                 of elements in `x.values`.
//
// @param units `string` containing the units for the legend (e.g. `"ha"`).
//
// @return `true` indicating success.
function continuousLegend(el, x, units) {
  // initialize lenged
  let l = document.createElement("div");
  l.className = "continuous-legend";
  l.setAttribute('data-toggle', 'tooltip');
  l.setAttribute('data-placement', 'bottom');
  l.setAttribute('title', 'Continuous Legend Tooltip');
  l.setAttribute('delay', '{ "show": 10, "hide": 5000 }');

  // calculate label/ticks positions
  /// calculate percentages
  positions = x.values.map((z) => {
    return ((z - x.min_value) / (x.max_value - x.min_value)) * 100;
  });
  /// replace any NaN values with zero (caused by zero / zero)
  positions = positions.map((z) => isNaN(z) ? 0 : z);

  console.log("numeric value at bottom (0%) of colorbar: " + x.min_value);
  console.log("numeric value at top (100%) of colorbar: " + x.max_value);
  console.log("numeric value of tick labels")
  console.log(x.values);
  console.log("percentage positions of ticks along the colorbar");
  console.log(positions);

  // create color bar
  let colorbar = document.createElement("div");
  colorbar.className = "color-bar";
  let colorbar_background = "linear-gradient(180deg";
  for (let i = 0; i < x.colors.length; ++i) {
    colorbar_background +=
      `, ${x.colors[i]} ${positions[i]}%`;
  }
  colorbar_background += ")";
  colorbar.style.backgroundColor = x.colors[0];
  colorbar.style.backgroundImage = colorbar_background;
  
  // create container for color bar ticks and labels
  let items = document.createElement("div");
  items.className = "items";
  //items.setAttribute('data-toggle', 'tooltip');
  //items.setAttribute('data-placement', 'bottom');
  //items.setAttribute('title', 'Continuous Legend Tooltip');
  //items.setAttribute('delay', '{ "show": 10, "hide": 5000 }');

  
  for (let i = 0; i < x.values.length; ++i) {
    // create container for i'th tick and label
    let item = document.createElement("div");
    item.className = "item";
    item.style.bottom = positions[i] + "%";

    // create tick
    let tick = document.createElement("div");
    tick.className = "tick";
    tick.innerText = " ";
    item.appendChild(tick);

    // create label
    let label = document.createElement("label");
    label.className = "colorbar-label";
    label.innerText = `${x.values[i]} ${units}`;
    item.appendChild(label);

    // append to container
    items.appendChild(item);
  }

  // construct legend
  l.appendChild(colorbar);
  l.appendChild(items);

  // add legend to parent container
  el.appendChild(l);
  
  
}
