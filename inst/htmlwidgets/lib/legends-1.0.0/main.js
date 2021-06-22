function createLegend(el, x, units) {
  if (x.type === "CategoricalLegend") {
    var y = categoricalLegend(el, x, units);
  } else if (x.type === "ContinuousLegend") {
    var y = continuousLegend(el, x, units);
  } else {
    console.error("unrecognized legend")
    console.log(x)
  }
  return y;
}
