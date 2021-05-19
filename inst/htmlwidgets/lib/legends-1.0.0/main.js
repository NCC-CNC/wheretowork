function createLegend(el, x, units) {
  if (x.type === "CategoricalLegend") {
    var y = categoricalLegend(el, x, units);
  } else {
    var y = continuousLegend(el, x, units);
  }
  return y;
}
