HTMLWidgets.widget({
  name: "importSettings",
  type: "output",
  factory: function(el, width, height) {
    // shared variables
    var elementId = el.id;
    var initialized = false;
    var container = document.getElementById(elementId);
    var layers_el = container.querySelector(".layers");
    var button_el = undefined;
    return {

      renderValue: function(opts) {
        // initialize widget
        if (!initialized) {
          /// set initialized
          initialized = true;
          /// attach the widget to the DOM
          container.widget = this;
          /// find button
          button_el = document.getElementById(opts.buttonId);
          /// add click event to button
          if (HTMLWidgets.shinyMode) {
            button_el.addEventListener("click", function() {
              /// compile message for backend
              const settings = el.querySelectorAll(".layer-settings");
              let i = 0;
              let value = {};
              Array.prototype.slice.call(settings).forEach((x) => {
                  ++i;
                  const v1 = x.querySelector("p").innerText;
                  const v2 = x.querySelector("input").checked;
                  const v3 = x.querySelector("select").value;
                  value[i] = {name: v1, import: v2, type: v3};
              });
              /// send message to backend
              Shiny.setInputValue(elementId, value);
            });
          }
          /// if initialized with values, then use them to populate widget
          if (opts.values !== null) {
            this.update(opts);
          }
        }
      },

      resize: function(width, height) {
        // widget automatically resizes
      },

      // export object for extensibility
      importSettings: container,

      /* API functions to manipulate widget */
      update: function(params) {
        // empty all layers
        this.empty();
        // promote to array if needed
        let values = params.value;
        if (typeof(values) === "string") {
          values = [params.value];
        }
        // add new layers based on parameters
        values.forEach((x) => {
          /// import template
          curr_el =
            document.importNode(
              document
              .getElementById(elementId)
              .querySelector(".layer-settings-template")
              .content,
            true);
          /// insert layer name into the label
          curr_el.querySelector("p").innerText = x;
          this.view_el = curr_el.querySelector("input");
          const select = curr_el.querySelector("select");
          const label = curr_el.querySelector("p");
          this.view_el.checked = true;
          curr_el.querySelector("select").disabled = false;
          /// add event listeners
          if (HTMLWidgets.shinyMode) {
            this.view_el.addEventListener("change", function () {
              let checked = this.checked;
              if (checked) {
                label.style.color = "black";
                select.disabled = false;
              } else {
                label.style.color = "#B8B8B8";
                select.disabled = true;
              }
             });
          }
          /// append layer to container
          layers_el.appendChild(curr_el);
          /// enable sorting within container
          this.sortable = new Sortable(
            layers_el, {
            animation: 150,
            dataIdAttr: "data-id",
            ghostClass: "ghost",
          });
        });
      },

      empty: function(params) {
        // remove all layers from layer container
        removeAllChildNodes(layers_el);
      },

    };
  }
});

// Attach message handlers if in Shiny mode (these correspond to API)
if (HTMLWidgets.shinyMode) {
  var fxns = ["update", "empty"];

  var addShinyHandler = function(fxn) {
    return function() {
      Shiny.addCustomMessageHandler(
        "importSettings:" + fxn, function(message) {
          var el = document.getElementById(message.id);
          if (el) {
            delete message["id"];
            el.widget[fxn](message);
          }
        }
      );
    }
  };
  for (var i = 0; i < fxns.length; ++i) {
    addShinyHandler(fxns[i])();
  }
}
