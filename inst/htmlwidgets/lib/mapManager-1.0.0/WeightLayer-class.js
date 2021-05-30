class WeightLayer {
  /* constructor */
  constructor(
    manager,
    id,
    name,
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
        .querySelector(".weight-layer-template")
        .content,
      true);
    this.view_el = this.el.querySelector(".view-checkbox");
    this.visible_el = this.el.querySelector(".visible-checkbox");
    this.name_el = this.el.querySelector(".name-label");
    this.legend_el = this.el.querySelector(".legend");

    // local variables
    let that = this;

    // attach id to element
    this.el.querySelector(".map-manager-layer").setAttribute("data-id", id);
    this.el.querySelector(".map-manager-layer").id = id;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// visible
    this.visible_el.checked = visible;
    /// view (i.e. show legend?), defaults to true
    this.view_el.checked = true;
    /// legend
    createLegend(this.legend_el, legend, units);

    // set listeners to update user interfance, show/hide legends checkbox
    if (HTMLWidgets.shinyMode) {
      this.view_el.addEventListener("change", function () {
        let checked = this.checked;
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
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "visible",
          value: checked
        });
      });
    }
  }

  /* update methods */
  updateParameter(parameter, value) {
    if (parameter === "name") {
      this.updateName(value);
    } else if (parameter === "visible") {
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
