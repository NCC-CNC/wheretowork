// define HTML widgets interface
HTMLWidgets.widget({
  name: "solutionSettings",
  type: "output",
  factory: function(el, width, height) {
    // top-level variables
    var elementId = el.id;
    var initialized = false;
    var container = document.getElementById(elementId);
    var handle = undefined;

    return {

      renderValue: function(opts) {
        if (!initialized) {
          // set initialized
          initialized = true;
          // initialize solution settings manaer
          handle = new SolutionSettings(
            elementId, container, opts.themes, opts.weights);
          // render HTML elements
          handle.render();
        }
      },

      resize: function(width, height) {
        // widget automatically resizes
      },

      // export object for extensibility
      solutionSettings: container,

      /* API functions to manipulate widget */
      // theme functions
      updateThemeName: function(params) {
        handle.updateThemeName(params.id, params.value);
      },

      updateThemeStatus: function(params) {
        handle.updateThemeStatus(params.id, params.value);
      },

      updateThemeView: function(params) {
        handle.updateThemeView(params.id, params.value);
      },

      updateThemeGroupGoal: function(params) {
        handle.updateThemeGroupGoal(params.id, params.value);
      },

      updateThemeFeatureGoals: function(params) {
        handle.updateThemeFeatureGoals(params.id, params.value);
      },

      updateThemeFeatureStatuses: function(params) {
        handle.updateThemeFeatureStatuses(params.id, params.value);
      },

      // weight functions
      updateWeightName: function(params) {
        handle.updateWeightName(params.id, params.value);
      },

      updateWeightStatus: function(params) {
        handle.updateWeightStatus(params.id, params.value);
      },

      updateWeightFactor: function(params) {
        handle.updateWeightFactor(params.id, params.value);
      }

    };
  }
});

// Attach message handlers if in Shiny mode (these correspond to API)
if (HTMLWidgets.shinyMode) {
  var fxns =
    ["updateThemeName", "updateThemeStatus", "updateThemeView",
     "updateThemeGroupGoal",
     "updateThemeFeatureGoal", "updateThemeFeatureStatus",
     "updateWeightName", "updateWeightStatus", "updateWeightFactor"];

  var addShinyHandler = function(fxn) {
    return function() {
      Shiny.addCustomMessageHandler(
        "theme:goal:" + fxn, function(message) {
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
