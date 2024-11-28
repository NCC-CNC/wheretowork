class Parameters {
  /* constructor */
  constructor(
    manager,
    ids,
    names,
    statuses,
    values,
    min_values,
    max_values,
    step_values,
    hides,
    units
  ) {
    // initialize HTML element to display the results
    this.el = document.createElement("div");
    this.el.classList.add("parameters-columns");
    
    // settings to show in summary table
    let show = ["Total area budget", "Spatial clustering", "Override includes"]
    let show_idx = ["budget_parameter", "spatial_parameter", "overlap_parameter"]

    // create elements
    const names_el = document.createElement("div");
    names_el.classList.add("names");
    const values_el = document.createElement("div");
    values_el.classList.add("values");
    
    // append each name to names element
    names.forEach((x) => {
      if (show.includes(x)) {
        const l = document.createElement("label");
        l.innerText = `${x}:`;
        names_el.appendChild(l);
      }
    });

    // append each value to values element
    let v = undefined;
    values.forEach((x, index) => {
      if (show_idx.includes(ids[index])) {
        /// initialize value text
        const l = document.createElement("label");
        l.innerText = "";
        /// add text for value
        if (hides[index] && !statuses[index]) {
          /// add text to indicate that parameter was not specified
          l.innerText += "Not specified"
        } else if (hides[index] && (["overlap_parameter"].includes(ids[index]))) {
          l.innerText += "On"
        } else {
          /// add number
          v = values[index] * statuses[index];
          l.innerText += `${roundToDigits(v, 2).toLocaleString("en-US")}`;
          /// add units
          if (units[index] === "%") {
            l.innerText += "%"
          } else {
            l.innerText += " " + units[index];
          }
        }
        /// add label to HTML element
        values_el.appendChild(l);
       }  
    });

    // append names and values to main element
    this.el.appendChild(names_el);
    this.el.appendChild(values_el);
  }

  /* render method */
  render() {
    return this.el;
  }

};
