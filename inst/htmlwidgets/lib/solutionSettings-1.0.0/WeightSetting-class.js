class WeightSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    min_factor,
    max_factor,
    initial_factor,
    step_factor,
    initial_status
  ) {
    // class fields
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

    // attach metadata to DOM
    this.el.querySelector(".solution-setting")
      .setAttribute("setting-id", id);
    this.el.querySelector(".solution-setting")
      .setAttribute("setting-name", name);

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// status
    this.status_el.checked = initial_status;
    /// factor
    noUiSlider.create(this.factor_el, {
      start: initial_factor,
      step: step_factor,
      connect: "lower",
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
        if (checked) {
          that.name_el.removeAttribute("disabled");
          that.factor_el.removeAttribute("disabled");
        } else {
          that.name_el.setAttribute("disabled", "");
          that.factor_el.setAttribute("disabled", "");
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// factor
      this.factor_el.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "factor",
          value: values[handle],
          type: "weight"
        });
      });
      /// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "status",
          value: checked,
          type: "weight"
        });
      });
    }
  }

  /* update HTML components */
  updateName(value) {
    this.name_el.innerText = value;
  }

  updateStatus(value) {
    let event = document.createEvent("HTMLEvents");
    this.status_el.checked = value;
    event.initEvent("change", false, true);
    this.status_el.dispatchEvent(event);
  }

  updateFactor(value) {
    this.factor_el.noUiSlider.set(value);
  }

  render() {
    return this.el;
  }

};
