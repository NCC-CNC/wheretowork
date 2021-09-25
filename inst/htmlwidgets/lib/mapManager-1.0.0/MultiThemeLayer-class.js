class MultiThemeLayer {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_id,
    feature_name,
    feature_visible,
    feature_hidden,
    feature_legend,
    feature_provenance,
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
    this.feature_hidden = feature_hidden;

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
    const that = this;

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
    const single_view_el = this.main_el.querySelectorAll(".view-checkbox");
    const single_name_el = this.main_el.querySelectorAll(".name-label");
    const single_prov_el =
      this.main_el.querySelectorAll(".provenance-container");
    const mapManagerLayer = this.el.querySelector(".map-manager-layer");

    // attach id to elements
    /// main container
    mapManagerLayer.setAttribute("data-id", id);
    mapManagerLayer.id = id;
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
      onUpdate: function() {
        if (HTMLWidgets.shinyMode) {
          const new_ids = this.toArray();
          const order = feature_id.map(function(x) {
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
    /// provenance
    const unique_provs = getUniqueBy(feature_provenance, "name");
    let prov_el = this.el.querySelector(".header .provenance-container");
    unique_provs.forEach((x) => createProvenance(prov_el, x));

    // set values for each feature
    let curr_prov_el = undefined;
    let curr_prov_icon_el = undefined;
    for (let i = 0; i < this.n_features; ++i) {
      /// name
      single_name_el[i].innerText = feature_name[i];
      /// visible
      this.single_visible_el[i].checked = feature_visible[i];
      /// view (i.e. show legend?), defaults to true
      single_view_el[i].checked = true;
      /// legend
      createLegend(this.single_legend_el[i], feature_legend[i], units);
      /// provenance
      createProvenance(single_prov_el[i], feature_provenance[i]);
      /// feature_hidden
      if (feature_hidden[i]) {
        this.main_el.children[i].classList.add("hidden-layer");
        this.main_el.children[i].setAttribute("disabled", "");
        single_view_el[i].checked = false;
        single_view_el[i].setAttribute("disabled", "");
        single_view_el[i].parentElement.classList.add("no-click");
        removeAllTooltips(single_view_el[i].parentElement);
        this.single_visible_el[i].setAttribute("disabled", "");
        this.single_visible_el[i].parentElement.classList.add("no-click");
        removeAllTooltips(this.single_visible_el[i].parentElement);
        this.single_legend_el[i].style.display = "none";
        removeAllTooltips(this.single_legend_el[i]);
      }
    }

    // set feature hidden for overall widget if all features are hidden
    if (feature_hidden.every((x) => x)) {
        mapManagerLayer.classList.add("hidden-layer");
        mapManagerLayer.setAttribute("disabled", "");
        this.visible_el.setAttribute("disabled", "");
        this.visible_el.parentElement.classList.add("no-click");
        removeAllTooltips(this.visible_el.parentElement);
        addHiddenTooltip(mapManagerLayer);
    }

    // set listeners to update user interface
    if (HTMLWidgets.shinyMode) {
      /// show/hide main container
      this.view_el.addEventListener("change", function () {
        const checked = this.checked;
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
          const checked = this.checked;
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
      /// visible button clicked
      this.visible_el.addEventListener("change", function () {
        const checked = this.checked;
        for (let i = 0; i < that.n_features; ++i) {
          that.single_visible_el[i].checked = checked;
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// overall visible button
      this.visible_el.addEventListener("change", function () {
        const checked = this.checked;
        that.single_visible_values = Array(that.n_features).fill(checked);
        Shiny.setInputValue(manager, {
          id: id,
          setting: "feature_visible",
          value: that.single_visible_values
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
    const value2 = new Array(this.n_features).fill(value);
    this.updateFeatureVisible(value2);
  }

  updateFeatureVisible(value) {
    const value2 = value.map((x, i) => x && !this.feature_hidden[i]);
    this.single_visible_values = value2;
    if (value2.some((x) => x) && (!this.visible_el.checked)) {
      this.visible_el.checked = true;
    }
    for (let i = 0; i < this.n_features; ++i) {
      this.single_visible_el[i].checked = value2[i];
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
