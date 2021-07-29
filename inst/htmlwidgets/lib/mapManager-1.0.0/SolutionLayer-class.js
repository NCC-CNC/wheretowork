class SolutionLayer {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    statistics,
    visible,
    legend,
    units
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".solution-layer-template")
        .content,
      true);
    this.view_el = this.el.querySelector(".view-checkbox");
    this.visible_el = this.el.querySelector(".visible-checkbox");
    this.name_el = this.el.querySelector(".name-label");
    this.remove_el = this.el.querySelector(".remove-button");
    this.legend_el = this.el.querySelector(".legend");

    // local variables
    const that = this;
    const mapManagerLayer = this.el.querySelector(".map-manager-layer")

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

    // set listeners to update user interfance
    if (HTMLWidgets.shinyMode) {
      this.view_el.addEventListener("change", function () {
        const checked = this.checked;
        if (checked) {
          that.legend_el.style.display = "block";
        } else {
          that.legend_el.style.display = "none";
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
        });
      });
      /// remove button
      this.remove_el.addEventListener("click", function () {
        /// remove all tooltips
        /// (ideally we would ONLY remove tooltips for this layer
        ///  but this cludge will suffice for now)
        const els  = document.querySelectorAll("[role='tooltip']");
        els.forEach((x) => x.parentElement.removeChild(x));
        /// tell backend to remove layer
        Shiny.setInputValue(manager, {
          id: id,
          setting: "remove",
        });
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
    this.visible_el.checked = value;
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  /* render method */
  render() {
    return this.el;
  }

};
