class WeightSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    min_factor,
    max_factor,
    factor,
    step_factor,
    status
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".weight-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.status_el = this.el.querySelector(".status-checkbox");
    this.factor_el = this.el.querySelector(".noUiSlider-widget");

    // local variables
    let that = this;

    // attach id to element
    this.el.querySelector(".solution-setting").id = id;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// status
    this.status_el.checked = status;
    /// factor
    noUiSlider.create(this.factor_el, {
      start: factor,
      step: step_factor,
      connect: "lower",
      tooltips: true,
      format: wNumb({decimals: 0}),
      range: {
        "min": min_factor,
        "max": max_factor
      }
    });

    // set listeners to update user interfance
    /// enable/disable widget on click
    if (HTMLWidgets.shinyMode) {
      this.status_el.addEventListener("change", function () {
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
      /// factor
      this.factor_el.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager, {
          id: id,
          setting: "factor",
          value: parseFloat(values[handle]),
          type: "weight"
        });
      });
      /// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "status",
          value: checked,
          type: "weight"
        });
      });
    }
  }

  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "status") {
      this.updateStatus(value);
    } else if (setting === "factor") {
      this.updateFactor(value);
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  updateStatus(value) {
    this.status_el.checked = value;
    let els =
      document.getElementById(this.id).querySelectorAll(
        ".disable-if-inactive, .disable-if-inactive.icon i");
    if (value) {
      els.forEach((x) => x.removeAttribute("disabled"));
    } else {
      els.forEach((x) => x.setAttribute("disabled", ""));
    }
  }

  updateFactor(value) {
    this.factor_el.noUiSlider.set(value);
  }

  /* render method */
  render() {
    return this.el;
  }

};
