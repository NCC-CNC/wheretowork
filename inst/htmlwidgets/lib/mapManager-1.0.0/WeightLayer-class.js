class WeightLayer {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    initial_visible,
    units,
    legend
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
    this.el.querySelector(".map-manager-layer").id = id;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// visible
    this.visible_el.checked = initial_visible;
    /// view (i.e. show legend?), defaults to false
    this.view_el.checked = false;
    /// legend
    createLegend(this.legend_el, legend, units);

    // set listeners to update user interfance
    /// enable/disable widget on click
    if (HTMLWidgets.shinyMode) {
      this.visible_el.addEventListener("change", function () {
        let checked = this.checked;
        let els =
          document.getElementById(id).querySelectorAll(
            ".disable-if-inactive");
        if (checked) {
          els.forEach((x) => x.removeAttribute("disabled"));
        } else {
          els.forEach((x) => x.setAttribute("disabled", ""));
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
