class Weight {
  /* constructor */
  constructor(
    manager, id, name, min_factor, max_factor, initial_factor, step_factor,
    initial_status) {
    // class fields
    this.el = document.getElementById("Weight-template").cloneNode(true);
    this.name_node = this.el.querySelector(".name-label");
    this.status_node = this.el.querySelector(".status-checkbox");
    this.factor_node = this.el.querySelector(".noUiSlider-widget");

    // local variables
    let that = this;

    // attach metadata to DOM
    this.el.setAttribute("setting-id", id);
    this.el.setAttribute("setting-name", name);

    // set initial values
    /// name
    this.name_node.innerText = name;
    /// status
    this.status_node.checked = initial_status;
    /// factor
    noUiSlider.create(this.factor_node, {
      start: initial_factor,
      step: step_factor,
      connect: "lower",
      range: {
        "min": min_factor,
        "max": max_factor
      },
    });

    // set listeners to update user interfance
    /// enable/disable widget on click
    if (HTMLWidgets.shinyMode) {
      this.status_node.addEventListener("change", function () {
        let checked = this.checked;
        if (checked) {
          that.name_node.removeAttribute("disabled");
          that.factor_node.removeAttribute("disabled");
        } else {
          that.name_node.setAttribute("disabled", "");
          that.factor_node.setAttribute("disabled", "");
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// factor
      this.factor_node.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "factor",
          value = values[handle],
          type = "weight"
        });
      });
      /// status
      this.status_node.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "status",
          value = checked,
          type = "weight"
        });
      });
    }
  }

  /* update HTML components */
  updateName(value) {
    this.name_node.innerText = value;
  }

  updateStatus(value) {
    let event = document.createEvent("HTMLEvents");
    this.status_node.checked = value;
    event.initEvent("change", false, true);
    this.status_node.dispatchEvent(event);
  }

  updateFactor(value) {
    this.factor_node.noUiSlider.set(value);
  }

  render() {
    return this.el;
  }

};
