class ParameterSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    status, // switch value
    value,
    min_value,
    max_value,
    step_value,
    hide,
    disable,
    units,
    reference_value,
    reference_units,
    tool_tip
  ) {
    
    // class fields
    this.id = id;
    this.elementId = "setting-" + id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".parameter-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.ref_el = this.el.querySelector(".reference-label");
    this.status_el = this.el.querySelector(".status-checkbox");
    this.value_el = this.el.querySelector(".noUiSlider-widget");
    this.value_container_el = this.el.querySelector(".parameter-slider");
    this.file_container_el = this.el.querySelector(".parameter-fileinput");
    this.previous_value = value;
    this.hide = hide;
    this.disable = disable;
    this.tool_tip = tool_tip;
    
    // local variables
    let that = this;

    // attach id to element
    this.el.querySelector(".solution-setting").id = this.elementId;
    
    // set initial values
    /// name
    this.name_el.innerText = name;
    /// value
    let slider_format = wNumb({decimals: 0, suffix: units});
    noUiSlider.create(this.value_el, {
      start: value,
      step: step_value,
      connect: "lower",
      tooltips: true,
      format: slider_format,
      range: {
        "min": min_value,
        "max": max_value
      }
    });
    /// status
    this.updateStatus(status);
    /// ref label
    this.has_ref = typeof(reference_value) === "number";
    if (this.has_ref) {
      let ref_units = reference_units.length > 0 ? " " + reference_units : "";
      let num_format = wNumb({decimals: 2, thousand: ","});
      this.ref_format = {
        // 'to' the formatted value. Receives a number.
        to: function (value) {
          return "(" + num_format.to(value * reference_value) + ref_units + ")";
        }
      }
      this.ref_el.innerText = this.ref_format.to(value);
    } else {
      this.ref_el.style.display = "none";
    }
    
    // create file icon
    if (id === "fileinput_parameter") {
     createFile(this.el.querySelector(".file-container")); 
    }

    /// show slider if budget or spatial parameter
    let sliders = ["budget_parameter", "spatial_parameter"] 
    if (status && sliders.includes(this.id)) { 
      //// show slider
      this.value_container_el.style.display = "block";
      if (this.has_ref) {
        //// show reference label
        this.ref_el.style.display = "inline";
      }
    } else {
      //// hide slider
      this.value_container_el.style.display = "none";
      if (this.has_ref) {
        //// hide reference label
        this.ref_el.style.display = "none";
      }
    }
    
    /// show file input if fileinput_parameter
    let fileinputs = ["fileinput_parameter"]
    if (status && fileinputs.includes(this.id)) {
      //// show input
      this.file_container_el.style.display = "block";
    } else {
      //// hide input
      this.file_container_el.style.display = "none";
    }    

    // set listeners to update user interface
    /// enable/disable widget on click
    if (HTMLWidgets.shinyMode) {
      this.status_el.addEventListener("change", function () {
        //// set switch value
        let checked = this.checked;
        //// update slider
        if (checked) {
          that.value_el.noUiSlider.set(that.previous_value);
          if (that.has_ref) {
            that.ref_el.innerText =
              that.ref_format.to(slider_format.from(that.previous_value));
          }
        } else {
          that.previous_value = that.value_el.noUiSlider.get();
          that.value_el.noUiSlider.set(min_value);
          if (that.has_ref) {
            that.ref_el.innerText = that.ref_format.to(min_value);
          }
        }
        /// hide slider if needed
        if (that.hide) {
          /// show slider if budget or spatial parameter
          if (checked && sliders.includes(id)) {
            //// show slider
            that.value_container_el.style.display = "block";
            if (that.has_ref) {
              //// show reference label
              that.ref_el.style.display = "inline";
            }
          } else {
            //// hide slider
            that.value_container_el.style.display = "none";
            if (that.has_ref) {
              //// hide reference label
              that.ref_el.style.display = "none";
            }
          }
          
          /// show file input if fileinput_parameter
          if (checked && fileinputs.includes(id)) {
            //// show input
            that.file_container_el.style.display = "block";
          } else {
            //// hide input
            that.file_container_el.style.display = "none";
          }             
        }
        
        //// update HTML styles
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

    // set listeners to accept file input configs .yaml
    const inputConfig = this.el.querySelector(".input_config")
    if (HTMLWidgets.shinyMode) {
      inputConfig.onchange = function(evt) {
        /// read file input as text and pass to R
        let reader = new FileReader();
        reader.onload = function(evt) {        
          let filecontent = evt.target.result;
          // pass data to Shiny  
          Shiny.setInputValue(manager, {
            id: id,
            setting: "fileinput",
            value: filecontent,
            type: "parameter"
          });
          // update file icon to green and tool tip
          let fileIcon_el = document.querySelector(".file-container i")
          fileIcon_el.style.color = "#33862B";
          $(fileIcon_el).attr('title', "Solution settings successfully updated.")
          .tooltip('fixTitle');          
        };
      // update file icon to grey and tool tip
      let fileIcon_el = document.querySelector(".file-container i")
      fileIcon_el.style.color = "#B8B8B8";
      $(fileIcon_el).attr('title', "No .yaml file submitted.")
      .tooltip('fixTitle');        
       reader.readAsText(evt.target.files[0]);
     };
    };
    
    if (HTMLWidgets.shinyMode) {
      /// value
      this.value_el.noUiSlider.on("update", function (values, handle) {
        let v = slider_format.from(values[handle]);
        if (that.has_ref) {
          that.ref_el.innerText = that.ref_format.to(v);
        }
        Shiny.setInputValue(manager, {
          id: id,
          setting: "value",
          value: v,
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
    // update HTML elements if needed
    if (this.status_el.checked !== value) {
      /// update switch
      this.status_el.checked = value;
      if (value) {
        /// enable label styles
        this.name_el.removeAttribute("disabled");
        // enable slider styles
        this.value_el.removeAttribute("disabled", "");
        this.value_el.noUiSlider.set(this.previous_value);
        if (this.has_ref) {
          this.ref_el.innerText =
            this.ref_format.to(slider_format.from.from(this.previous_value));
        }
      } else {
        /// disable name styles
        this.name_el.setAttribute("disabled", "");
        /// disable slider styles
        this.value_el.setAttribute("disabled", "");        
        this.previous_value = this.value_el.noUiSlider.get();
        this.value_el.noUiSlider.set(1); // set min value 
        if (this.has_ref) {
          this.ref_el.style.display = "none";
          this.ref_el.innerText = this.ref_format.to(1); // set min value
        }
      }
      
      if (this.hide) {
        /// show slider if budget or spatial parameter
        const sliders = ["budget_parameter", "spatial_parameter"] 
        if (this.status_el.checked && sliders.includes(this.id)) {
          /// show slider
          this.value_container_el.style.display = "block";
          if (this.has_ref) {
            //// show reference label
            this.ref_el.style.display = "inline";
          }
        } else {
          /// hide slider
          this.value_container_el.style.display = "none";
          if (this.has_ref) {
            //// hide reference label
            this.ref_el.style.display = "none";
          }
        }
      } 
    }
    
    // update HTML element styles
    let els = this.el.querySelectorAll(
        ".disable-if-inactive, .disable-if-inactive.icon i");
    if (value) {
      els.forEach((x) => x.removeAttribute("disabled"));
    } else {
      els.forEach((x) => x.setAttribute("disabled", ""));
    }
  }

  updateValue(value) {
    this.previous_value = value;
    this.value_el.noUiSlider.set(value);
    if (this.has_ref) {
      this.ref_el.innerText = this.ref_format.to(value);
    }
  }
  
  /* render method */
  render() {
    return this.el;
  }

};
