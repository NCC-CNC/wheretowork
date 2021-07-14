class Statistics {
  /* constructor */
  constructor(
    manager,
    names,
    values,
    units) {
    // initialize HTML element to display the results
    this.el = document.createElement("div");
    this.el.classList.add("statistics-results");

    // create elements
    const names_el = document.createElement("div");
    names_el.classList.add("names");
    const values_el = document.createElement("div");
    values_el.classList.add("values");

    // append each name to names element
    names.forEach((x) => {
      const l = document.createElement("label");
      l.innerText = `${x}:`;
      names_el.appendChild(l);
    });

    // append each value to values element
    values.forEach((x, index) => {
      const l = document.createElement("label");
      l.innerText = `${roundToDigits(values[index], 2)} ${units[index]}`;
      values_el.appendChild(l);
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
