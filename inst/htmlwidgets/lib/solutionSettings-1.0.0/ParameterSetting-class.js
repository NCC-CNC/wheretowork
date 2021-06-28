class ParameterSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    min_value,
    max_value,
    value,
    step_value,
    status,
    units
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".parameter-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.status_el = this.el.querySelector(".status-checkbox");
    this.value_el = this.el.querySelector(".noUiSlider-widget");

    // local variables
    let that = this;

    // attach id to element
    this.el.querySelector(".solution-setting").id = id;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// status
    this.status_el.checked = status;
    /// value
    noUiSlider.create(this.value_el, {
      start: value,
      step: step_value,
      connect: "lower",
      tooltips: true,
      format: wNumb({decimals: 0}),
      range: {
        "min": min_value,
        "max": max_value
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
      /// value
      this.value_el.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager, {
          id: id,
          setting: "value",
          value: parseFloat(values[handle]),
          type: "parameter"
        });
      });
      /// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "status",
          value: checked,
          type: "parameter"
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
    } else if (setting === "value") {
      this.updateValue(value);
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

  updateValue(value) {
    this.value_el.noUiSlider.set(value);
  }

  /* render method */
  render() {
    return this.el;
  }

};
