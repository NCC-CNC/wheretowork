class Statistics {
  /* constructor */
  constructor(
    manager,
    names,
    values,
    units,
    proportions
  ) {

    // initialize HTML element to display the results
    this.el = document.createElement("div");
    this.el.classList.add("statistics-columns");

    // create elements
    const names_el = document.createElement("div");
    names_el.classList.add("names");
    const values_el = document.createElement("div");
    values_el.classList.add("values");
    const proportions_el = document.createElement("div");
    proportions_el.classList.add("values");

    // append each name to names element
    names.forEach((x) => {
      const l = document.createElement("label");
      l.innerText = `${x}:`;
      names_el.appendChild(l);
    });

    // append each value to values element
    values.forEach((x, index) => {
      /// initialize value text
      const l = document.createElement("label");
      l.innerText = "";
      /// add text for value
      if (typeof(values[index]) === "number") {
        /// add number
        l.innerText +=
          `${roundToDigits(values[index], 2).toLocaleString("en-US")}`;
        /// add units
        if (units[index] === "%") {
          l.innerText += "%"
        } else {
          l.innerText += " " + units[index];
        }
      } else {
        l.innerText = values[index];
      }
      /// add label to HTML element
      values_el.appendChild(l);
    });

    // append each value to values element
    proportions.forEach((x, index) => {
      const l = document.createElement("label");
      if (typeof(x) === "number") {
        l.innerText = `(${roundToDigits(x * 100, 0)}%)`;
      } else {
        l.innerText = "TEXT";
        l.style.visibility = "hidden"
      }
      proportions_el.appendChild(l);
    });

    // append names and values to main element
    this.el.appendChild(names_el);
    this.el.appendChild(values_el);
    this.el.appendChild(proportions_el);
  }

  /* render method */
  render() {
    return this.el;
  }

};
