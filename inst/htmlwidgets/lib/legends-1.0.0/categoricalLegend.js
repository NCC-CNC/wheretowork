// Create a categorical legend
//
// This function is used to create a categorical legend and insert it
// into a parent HTML element (i.e. `div` container).
//
// @param el HTML element within which to insert the legend.
//
// @param x `Array` containing the following `Array` elements.
//   Note that each of these elements are assumed to have exactly the same
//   number of elements, such that `x.values.length === x.colors.length`.
//   elements are assumed to have exactly the same number  :
//   * `x.values`: `Array` of `number` objects for each item in the legend.
//   * `x.colors`: `Array` of `string` objects containing colors
//                 (e.g. `"#112233"`) for each item within the legend.
//
// @param units `string` containing the units for the legend (e.g. `"ha"`).
//
// @return `true` indicating success.
function categoricalLegend(el, x, units) {
  // initialize lenged
  let l = document.createElement("div");
  l.className = "categorical-legend";
  l.setAttribute("data-toggle", "tooltip");
  l.setAttribute("data-placement", "bottom");
  l.setAttribute("data-delay", "{\"show\":500, \"hide\":100}");
  l.setAttribute("data-container", "sidebar");
  l.setAttribute(
    "title",
    "This dataset has categorical data. " +
    "The legend shows the color associated with each category.");

  // create legend
  for (let i = 0; i < x.values.length; ++i) {
    /// create item to add to legend
    let item = document.createElement("div");
    item.className = "item";

    /// create text component to dispay the text
    let item_label = document.createElement("label");
    item_label.className = "item-label disable-if-inactive";
    item_label.innerText = `${x.values[i]} ${units}`;

    /// create symbol component to display the color
    let item_symbol = document.createElement("div");
    item_symbol.className = "item-symbol disable-if-inactive";
    item_symbol.style.backgroundColor = x.colors[i];
    item_symbol.style.color = x.colors[i];
    if (x.colors[i].length === 9) {
      // if has alpha channel, then set opacity
      var r = parseInt(x.colors[i].slice(1, 3), 16);
      var g = parseInt(x.colors[i].slice(3, 5), 16);
      var b = parseInt(x.colors[i].slice(5, 7), 16);
      var a = 1 - parseInt(x.colors[i].substr(-2), 16) / 255;
      item_symbol.style.backgroundColor = `rgba(${r}, ${g}, ${b}, ${a})`;
      item_symbol.style.color = `rgba(${r}, ${g}, ${b}, ${a})`;
    }

    /// add item to legend
    item.appendChild(item_symbol);
    item.appendChild(item_label);
    l.appendChild(item);
  }

  // add legend to parent container
  el.appendChild(l);

  // return true
  return true;
}
