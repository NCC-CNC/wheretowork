class IncludeLayer {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    visible,
    hidden,
    legend,
    units,
    provenance
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".include-layer-template")
        .content,
      true);
    this.view_el = this.el.querySelector(".view-checkbox");
    this.visible_el = this.el.querySelector(".visible-checkbox");
    this.name_el = this.el.querySelector(".name-label");
    this.legend_el = this.el.querySelector(".legend");
    this.hidden = hidden;

    // local variables
    const that = this;
    const mapManagerLayer = this.el.querySelector(".map-manager-layer");

    // attach id to element
    mapManagerLayer.setAttribute("data-id", id);
    mapManagerLayer.id = id;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// visible
    this.visible_el.checked = visible;
    /// view (i.e. show legend?), defaults to true
    this.view_el.checked = true;
    /// legend
    createLegend(this.legend_el, legend, units);
    /// provenance
    createProvenance(
      this.el.querySelector(".provenance-container"), provenance
    );
    /// hidden
    if (hidden) {
      mapManagerLayer.classList.add("hidden-layer");
      mapManagerLayer.setAttribute("disabled", "");
      this.view_el.checked = false;
      this.view_el.setAttribute("disabled", "");
      this.view_el.parentElement.classList.add("no-click");
      removeAllTooltips(this.view_el.parentElement);
      this.visible_el.setAttribute("disabled", "");
      this.visible_el.parentElement.classList.add("no-click");
      removeAllTooltips(this.visible_el.parentElement);
      that.legend_el.style.display = "none";
      removeAllTooltips(this.legend_el);
      addHiddenTooltip(mapManagerLayer);
    }

    // set listeners to update user interface, show/hide legends checkbox
    if (HTMLWidgets.shinyMode) {
      this.view_el.addEventListener("change", function () {
        const checked = this.checked;
        if (checked) {
          that.legend_el.style.display = "block";
          // TODO: insert JS to add animation for maximizing legend
        } else {
          that.legend_el.style.display = "none";
          // TODO: insert JS to add animation for minimizing legend
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// status
      this.visible_el.addEventListener("change", function () {
        const checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "visible",
          value: checked
        }, {priority: "event"});
      });
    }
  }

  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "visible") {
      this.updateVisible(value);
    }
  }

  updateVisible(value) {
    if (!this.hidden) {
      this.visible_el.checked = value;
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  /* render method */
  render() {
    return this.el;
  }

};
