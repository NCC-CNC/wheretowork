function createLegend(el, x, units) {
  if (x.type === "CategoricalLegend") {
    return categoricalLegend(el, x, units);
  } else if (x.type === "ContinuousLegend") {
    return continuousLegend(el, x, units);
  } else if (x.type === "ManualLegend") {
    return manualLegend(el, x)
  } else if (x.type === "NullLegend") {
    return null    
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
      x.units,
      x.hidden
    );
  } else if (x.type == "include") {
    return new IncludeLayer(
      manager,
      x.id,
      x.name,
      x.visible,
      x.hidden,
      x.legend,
      x.units,
      x.provenance
    );
  } else if (x.type == "exclude") {
    return new ExcludeLayer(
      manager,
      x.id,
      x.name,
      x.visible,
      x.hidden,
      x.legend,
      x.units,
      x.provenance
    );    
  } else if (x.type == "weight") {
    return new WeightLayer(
      manager,
      x.id,
      x.name,
      x.visible,
      x.hidden,
      x.legend,
      x.units,
      x.provenance
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
        x.feature_hidden,
        x.feature_legend[0],
        x.feature_provenance[0],
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
        x.feature_hidden,
        x.feature_legend,
        x.feature_provenance,
        x.units
      );
    }
  }
}

function addHiddenTooltip(el) {
  el.setAttribute("data-toggle", "tooltip");
  el.setAttribute("data-placement", "bottom");
  el.setAttribute("data-container", "body");
  el.setAttribute("data-trigger", "hover");
  el.setAttribute("title", "Data cannot be shown on map");
}

function removeAllTooltips(el) {
  // remove attributes from element
  el.removeAttribute("data-toggle");
  el.removeAttribute("data-placement");
  el.removeAttribute("data-delay");
  el.removeAttribute("data-container");
  el.removeAttribute("title");
  // remove attributes from children
  el
  .querySelectorAll("[data-toggle='tooltip']")
  .forEach((x) => {
    x.removeAttribute("data-toggle");
    x.removeAttribute("data-placement");
    x.removeAttribute("data-delay");
    x.removeAttribute("data-container");
    x.removeAttribute("title");
  });
}
