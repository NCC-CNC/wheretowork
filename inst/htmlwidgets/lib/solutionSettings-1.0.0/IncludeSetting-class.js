class IncludeSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    status,
    overlap,
    mandatory,
    provenance
  ) {
    // class fields
    this.id = id;
    this.elementId = "setting-" + id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".include-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.status_el = this.el.querySelector(".status-checkbox");

    // local variables
    let that = this;

    // attach id to element
    this.el.querySelector(".solution-setting").id = this.elementId;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// status
    this.updateStatus(status);
    /// provenance
    createProvenance(
      this.el.querySelector(".provenance-container"), provenance
    );
    // create include overlap warning icon 
    if (overlap) {
      createWarning(
        this.el.querySelector(".warning-container"), name, "excludes", overlap
      );      
    }    

    // disable switches if include is mandatory
    if (mandatory) {
      this.status_el.parentElement.classList.add("disable-mouse");
      this.status_el.addEventListener("click", function (e) {
        e.preventDefault();
        return false;
      });
    }

    // set listeners to update user interface
    /// enable/disable widget on click
    if (HTMLWidgets.shinyMode) {
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        let els =
          document.getElementById(that.elementId).querySelectorAll(
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
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "status",
          value: checked,
          type: "include"
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
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  updateStatus(value) {
    this.status_el.checked = value;
    let els = this.el.querySelectorAll(
      ".disable-if-inactive, .disable-if-inactive.icon i");
    if (value) {
      els.forEach((x) => x.removeAttribute("disabled"));
    } else {
      els.forEach((x) => x.setAttribute("disabled", ""));
    }
  }

  /* render method */
  render() {
    return this.el;
  }

};
