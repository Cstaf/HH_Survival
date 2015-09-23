function Survdiagram(render, titel, subtitel, height,width,marginBottom, legend ,ser) {
     var chart = new Highcharts.Chart({
           chart: {   
             type: 'line',
             renderTo : render, 
             marginBottom: marginBottom, 
             height: height, 
             width: width, 
             style: { 
             fontSize: '12px'} 
           }, 
           credits: false,   
           title: {   
               text: titel, 
                style: { 
               fontSize: '14px'} 
           }, 
           subtitle: { 
            text: subtitel, 
            style: { 
                 fontSize: '12px'} 
          },   
           xAxis: {
              min: 0,
              max: 5,
              allowDecimals: false,   
              title: {  
                text: 'År efter diagnos'}, 
              labels: { 
                style: { 
                    fontSize:'10px'} 
                   }   
           },   
           yAxis: {   
               min: 0,
               max: 100, 
               gridLineColor: '#EFEFEF',   
               title: {   
                   text: 'Överlevnad (%)'},   
               stackLabels: {   
                   enabled: true,   
                   style: {   
                       fontWeight: 'bold',   
                       color: (Highcharts.theme && Highcharts.theme.textColor) || 'gray', 
                       fontSize: '10px'}   
               } 
           },   
           legend: {
               enabled: legend,   
               align: 'right',   
               x: 0,   
               verticalAlign: 'bottom',   
               y: -10,   
               floating: true,   
               backgroundColor: (Highcharts.theme && Highcharts.theme.background2) || 'white',   
               borderColor: '#CCC',   
               borderWidth: 1,   
               shadow: false, 
               symbolHeight: 8, 
               itemStyle: { 
                  fontSize:'12px'}   
           },
          lang: {
              noData: "Det finns inga patienter för rapporten/mallen <br> för givna parametrar, var god försök igen med andra <br> parameterval. Om problemet kvarstår var god <br> kontakta ansvarig nationell statistiker"
          },
            noData: {
              style: {
                fontWeight: 'bold',
                fontSize: '12px',
                color: '#999999'
            }
          }, 
           exporting: { 
           enabled: false,  
            sourceHeight: 768,  
            sourceWidth: 1024  
           },     
           tooltip: {
             headerFormat: headerFormat,  
             pointFormat: 'År efter diagnos: <b>{point.x}</b><br> Överlevnad (95% KI): <b>{point.y}% {point.ci}</b><br> Antal i risk: <b>{point.nrisk}</b> <br/>', 
             shared: false,
             useHTML: true  
           },   
           plotOptions: {
               series:{
                  turboThreshold: 50000,
                  lineWidth: 3   
               },   
               column: {   
                   stacking: 'normal',   
                   dataLabels: {   
                       enabled: true,   
                       color: (Highcharts.theme && Highcharts.theme.dataLabelsColor) || 'white',   
                       style: {   
                           textShadow: '0 0 3px black', 
                           fontSize: '8px'}   
                   }   
               }   
           },   
          series: ser

         });   
   }; 