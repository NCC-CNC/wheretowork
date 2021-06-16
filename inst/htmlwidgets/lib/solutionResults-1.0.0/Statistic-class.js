class Statistic {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    value,
    units) {
    // initialize HTML element to display the results
    this.id = id;
    this.chart = undefined;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".statistic-template")
        .content,
      true);


    // prepare HTML element
    /// assign id to chart HTML element
    this.el.querySelector(".solution-result").id = id;

    // append statistics to the HTML element
    const l = document.createElement("label");
    l.innerText = `${name}: ${value} ${units}`;
    this.el.appendChild(l);

  }

  /* render method */
  render() {
    return this.el;
  }

  /* post render method */
  postrender() {
    // TODO: see ./SingleThemeResults-class.js for example
  }

  /* destroy method */
  destroy() {
    // TODO: see ./SingleThemeResults-class.js for example
  }

};
