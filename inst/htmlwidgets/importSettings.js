HTMLWidgets.widget({
  name: "importSettings",
  type: "output",
  factory: function(el, width, height) {
    // shared variables
    var elementId = el.id;
    var initialized = false;
    var container = document.getElementById(elementId);
    var layers_el = container.querySelector(".layers");
    var button_el = container.querySelector(`#${elementId}_button`);
    return {

      renderValue: function(opts) {
        if (!initialized) {
          // set initialized
          initialized = true;
          // add click event to button
          if (HTMLWidgets.shinyMode) {
            button_el.addEventListener("click", function() {
              // compile results
              const settings = el.querySelectorAll(".layer-settings");
              let i = 0;
              let value = {};
              Array.prototype.slice.call(settings).forEach((x) => {
                  ++i;
                  const v1 = x.querySelector("label").innerText;
                  const v2 = x.querySelector("input").checked;
                  const v3 = x.querySelector("select").value;
                  value[i] = {name: v1, import: v2, type: v3};
              });
              // send message to backend
              Shiny.setInputValue(elementId, value);
            });
          }
          // if opts.values contains values then use them to populate the widget
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

        // add new layers based on parameters
        params.values.forEach((x) => {
          // import template
          curr_el =
            document.importNode(
              document
              .getElementById(elementId)
              .querySelector(".layer-settings-template")
              .content,
            true);
          // insert layer name into the label
          curr_el.querySelector("label").innerText = x;

          this.view_el = curr_el.querySelector("input");
          var select = curr_el.querySelector("select");       
          var label = curr_el.querySelector("label");
          this.view_el.checked = true;
          curr_el.querySelector("select").disabled = false;

          // TODO: add event handler to
          // 1: disable select input + make label grey
          // when the curr_el.querySelector("input").checked === false;
          // 2: enable select input + make label black
          // when the curr_el.querySelector("input").checked === true;

          this.view_el.addEventListener("change", function () {
            let checked = this.checked;
            if (checked) {
              label.style.color = "black";
              select.disabled=false;
            } else {
              label.style.color = "grey";
              select.disabled = true;
            }
           });

          // append layer to container
          layers_el.appendChild(curr_el);
          // enable sorting within container
          this.sortable = new Sortable(
            layers_el, {
            animation: 150,
            dataIdAttr: "data-id",
            ghostClass: "ghost",
          });
        });
        // enable button
        button_el.removeAttribute("disabled");
      },

      empty: function(params) {
        // remove all layers from layer container
        removeAllChildNodes(layers_el);

        // set button as disabled
        button_el.setAttribute("disabled", "");
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
        "importSetings:" + fxn, function(message) {
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
