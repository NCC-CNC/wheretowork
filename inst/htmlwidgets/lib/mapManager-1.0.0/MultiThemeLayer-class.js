class MultiThemeLayer {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_id,
    feature_name,
    feature_visible,
    feature_legend,
    units
  ) {
    // class fields
    /// internal variables
    this.id = id;
    this.n_features = feature_id.length;
    this.feature_id = feature_id;
    this.single_visible_values = feature_visible;
    this.single_visible_el = undefined;
    this.single_legend_el = undefined;
    this.sortable = undefined;

    /// HTML container
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".multi-theme-layer-template")
        .content,
      true);
    this.view_el = this.el.querySelector(".view-checkbox");
    this.visible_el = this.el.querySelector(".visible-checkbox");
    this.name_el = this.el.querySelector(".name-label");
    this.main_el = this.el.querySelector(".main");

    // local variables
    let that = this;

    /// attach DOM elements for each feature
    for (let i = 0; i < this.n_features; ++i) {
      this.main_el.appendChild(
        document.importNode(
          document
          .getElementById(manager)
          .querySelector(".multi-theme-single-layer-template")
          .content,
        true));
    }

    /// store HTML elements for each feature
    this.single_visible_el = this.main_el.querySelectorAll(".visible-checkbox");
    this.single_legend_el = this.main_el.querySelectorAll(".legend");
    let single_view_el = this.main_el.querySelectorAll(".view-checkbox");
    let single_name_el = this.main_el.querySelectorAll(".name-label");

    // attach id to elements
    /// main container
        this.el.querySelector(".map-manager-layer").setAttribute("data-id", id);
    this.el.querySelector(".map-manager-layer").id = id;
    /// single view containers
    for (let i = 0; i < this.n_features; ++i) {
      this.main_el.children[i].id = feature_id[i];
      this.main_el.children[i].setAttribute("data-id", feature_id[i]);
    }
    /// enable sorting within main container
    this.sortable = new Sortable(
      this.main_el, {
      animation: 150,
      dataIdAttr: "data-id",
      ghostClass: "ghost",
      onUpdate: function(event) {
        if (HTMLWidgets.shinyMode) {
          let new_ids = this.toArray();
          let order = feature_id.map(function(x) {
            return that.n_features - (new_ids.findIndex((z) => z === x));
          });
          Shiny.setInputValue(manager, {
            id: id,
            setting: "feature_order",
            value: order
          });
        }
      }
    });

    // set overall theme values
    /// name
    this.name_el.innerText = name;
    /// visible
    this.visible_el.checked = feature_visible.some((x) => x);
    /// view (i.e. show legend?), defaults to true
    this.view_el.checked = true;

    // set values for each feature
    for (let i = 0; i < this.n_features; ++i) {
      /// name
      single_name_el[i].innerText = feature_name[i];
      /// visible
      this.single_visible_el[i].checked = feature_visible[i];
      /// view (i.e. show legend?), defaults to true
      single_view_el[i].checked = true;
      /// legend
      createLegend(this.single_legend_el[i], feature_legend[i], units);
    }

    // set listeners to update user interfance
    if (HTMLWidgets.shinyMode) {
      /// show/hide main container
      this.view_el.addEventListener("change", function () {
        let checked = this.checked;
        if (checked) {
          that.main_el.style.display = "block";
          // TODO: insert JS to add animation for maximizing container
        } else {
          that.main_el.style.display = "none";
          // TODO: insert JS to add animation for minimizing container
        }
      });
      /// show/hide legends
      for (let i = 0; i < this.n_features; ++i) {
        single_view_el[i].addEventListener("change", function () {
          let checked = this.checked;
          if (checked) {
            that.single_legend_el[i].style.display = "block";
            // TODO: insert JS to add animation for maximizing legend
          } else {
            that.single_legend_el[i].style.display = "none";
            // TODO: insert JS to add animation for minimizing legend
          }
        });
      }
      /// set all features to be visible/invisible when overall
      /// visisble button clicked
      this.visible_el.addEventListener("change", function () {
        let checked = this.checked;
        for (let i = 0; i < that.n_features; ++i) {
          that.single_visible_el[i].checked = checked;
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// overall visible button
      this.visible_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "feature_visible",
          value: Array(that.n_features).fill(checked),
        });
      });

      /// visible button for each feature
      for (let i = 0; i < this.n_features; ++i) {
        this.single_visible_el[i].addEventListener("change", function () {
          that.single_visible_values[i] = this.checked;
          Shiny.setInputValue(manager, {
            id: id,
            setting: "feature_visible",
            value: that.single_visible_values
          });
        });
      }
    }
  }

  /* update HTML elements */
  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "visible") {
      this.updateVisible(value);
    } else if (setting === "feature_visible") {
      this.updateFeatureVisible(value);
    } else if (setting === "feature_order") {
      this.updateFeatureOrder(value);
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  updateVisible(value) {
    this.single_visible_values.fill(value);
    this.visible_el.checked = value;
    for (let i = 0; i < this.n_features; ++i) {
      this.single_visible_el[i].checked = value;
    }
  }

  updateFeatureVisible(value) {
    this.single_visible_values = value;
    if (value.some((x) => x) && (!this.visible_el.checked)) {
      this.visible_el.checked = true;
    }
    for (let i = 0; i < this.n_features; ++i) {
      this.single_visible_el[i].checked = value[i];
    }
  }

  updateFeatureOrder(value) {
    // create array with ids in order
    const new_ids = new Array(this.n_features);
    for (let i = 0; i < new_ids.length; ++i) {
      new_ids[this.n_features - value[i]] = this.feature_id[i];
    }
    // re-order layers in widget
    this.sortable.sort(new_ids, true);
  }



  /* render method */
  render() {
    return this.el;
  }

};
