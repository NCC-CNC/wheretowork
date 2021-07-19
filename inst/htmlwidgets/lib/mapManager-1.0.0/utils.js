function createLegend(el, x, units) {
  if (x.type === "CategoricalLegend") {
    return categoricalLegend(el, x, units);
  } else if (x.type === "ContinuousLegend") {
    return continuousLegend(el, x, units);
  } else {
    console.error("unrecognized legend")
    console.log(x)
  }
}

function newLayer(manager, x) {
  if (x.type == "solution") {
    return new SolutionLayer(
      manager,
      x.id,
      x.name,
      x.statistics,
      x.visible,
      x.legend,
      x.units
    );
  } else if (x.type == "include") {
    return new IncludeLayer(
      manager,
      x.id,
      x.name,
      x.visible,
      x.legend,
      x.units
    );
  } else if (x.type == "weight") {
    return new WeightLayer(
      manager,
      x.id,
      x.name,
      x.visible,
      x.legend,
      x.units
    );
  } else {
    if (typeof(x.feature_name) === "string") {
      return new SingleThemeLayer(
        manager,
        x.id,
        x.name,
        x.feature_id,
        x.feature_name,
        x.feature_visible,
        x.feature_legend,
        x.units
      );
    } else {
      return new MultiThemeLayer(
        manager,
        x.id,
        x.name,
        x.feature_id,
        x.feature_name,
        x.feature_visible,
        x.feature_legend,
        x.units
      );
    }
  }
}
