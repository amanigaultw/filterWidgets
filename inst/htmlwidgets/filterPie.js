HTMLWidgets.widget({

  name: 'filterPie',

  type: 'output',

  factory: function(el, width, height) {

    var myChart = null;

    return {

      renderValue: function(x) {

        if (myChart === null) {
                    myChart = echarts.init(document.getElementById(el.id));
        } else {
                    myChart.dispose(document.getElementById(el.id));
                    myChart = echarts.init(document.getElementById(el.id));
        }

        var option;
        var extraOptions;

        extraOptions = x.options;


option = {
  tooltip: {
    trigger: 'item'
  },
  legend: {
    orient: 'vertical',
    left: 'left'
  },
  series: [
    {
      type: 'pie',
      radius: '50%',
      data: x.data,
      emphasis: {
        itemStyle: {
          shadowBlur: 10,
          shadowOffsetX: 0,
          shadowColor: 'rgba(0, 0, 0, 0.5)'
        }
      }
    }
  ],
  ...extraOptions
};

        myChart.on('click', 'series.pie', (params) => {
            console.log(params.data.name);
            if (HTMLWidgets.shinyMode) {
                Shiny.setInputValue(x.reactiveID, params.data.name);
            }
        });

        option && myChart.setOption(option, true);

      },

      resize: function(width, height) {

                if (!myChart)
                    return;

                myChart.resize({
                    width: width,
                    height: height
                });

      }

    };
  }
});
