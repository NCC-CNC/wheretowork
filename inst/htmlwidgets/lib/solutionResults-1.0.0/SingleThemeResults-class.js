class SingleThemeResults {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_name,
    feature_id,
    feature_status,
    feature_total_amount,
    feature_current_held,
    feature_goal,
    feature_solution_held,
    units,
    mandatory,
    round,
    icon
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".single-theme-results-template")
        .content,
      true);

    // prepare HTML element
    /// assign id to chart HTML element
    this.el.querySelector(".solution-result").id = id;

    // define chart options
    /// this is just an example to see if we can make charts
    /// please over-write this.chart_options field to update the chart as needed
    this.chart_options = {
      series: [Math.round(feature_solution_held * 100)],
      chart: {
      type: 'radialBar',
      offsetY: -20,
      sparkline: {
        enabled: true
      }
    },
    plotOptions: {
      radialBar: {
        startAngle: -90,
        endAngle: 90,
        track: {
          background: "#e7e7e7",
          strokeWidth: '97%',
          margin: 5, // margin is in pixels
          dropShadow: {
            enabled: true,
            top: 2,
            left: 0,
            color: '#999',
            opacity: 1,
            blur: 2
          }
        },
        dataLabels: {
          name: {
            show: false
          },
          value: {
            offsetY: -2,
            fontSize: '22px'
          }
        }
      }
    },
    grid: {
      padding: {
        top: -10
      }
    },
    fill: {
      type: 'gradient',
      gradient: {
        shade: 'light',
        shadeIntensity: 0.4,
        inverseColors: false,
        opacityFrom: 1,
        opacityTo: 1,
        stops: [0, 50, 53, 91]
      },
    },
    labels: ['Average Results'],
    };
  }

  /* render method */
  render() {
    return this.el;
  }

  /* post render method */
  postrender() {
    const node = document.querySelector(`[id="${this.id}"]`);
    const chart = new ApexCharts(node, this.chart_options);
    chart.render();
  }

};
