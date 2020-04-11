// Author: Wesley Cota <https://wesleycota.com/>


$(document).ready(function() {

    $.ajax({
        type: "GET",
        url: "https://raw.githubusercontent.com/wcota/covid19br/master/cases-gps.csv",
        dataType: "text",
        success: function(data) {processData(data);}
        });

    function processData(allText) {

        var allTextLines = allText.split(/\r\n|\n/);
        var headers = allTextLines[0].split(',');
        var casesData = [];

        for (var i=1; i<allTextLines.length; i++) {
            var data = allTextLines[i].split(',');
            if (data.length == headers.length) {
    
                var tarr = {};
                for (var j=0; j<headers.length; j++) {
                    tarr[headers[j]]  = data[j];
                }
                casesData.push(tarr);
            }
        }

        var createMap = function() {

            // Remove loading page
            document.getElementById("loader").style="display:none!important";

            // Create map
            var map = L.map('map',
            {
                zoomSnap: 0.2,
                gestureHandling: true,
                //zoomDelta: 0.4
            });
            map.createPane('labels');
            map.getPane('labels').style.zIndex = 650;
            map.getPane('labels').style.pointerEvents = 'none';

            var gps1 = [-34.25,-74.37];
            var gps2 = [5.61,-33.25];

            map.fitBounds([gps1, gps2]);

            var positron = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png?{foo}', {foo: 'bar', attribution: 'Map data &copy; <a target = "_parent" href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a target="_parent" href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'}).addTo(map);
            // var positronLabels = L.tileLayer('https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png', {
            //         attribution: '&copy; <a href="https://carto.com/">CartoDB</a>',
            //         maxNativeZoom: 12,
            //         pane: 'labels'
            // }).addTo(map);
            // var layer = L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}' + (L.Browser.retina ? '@2x.png' : '.png'), {
            //     attribution:'&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, &copy; <a href="https://carto.com/attributions">CARTO</a>',
            //     subdomains: 'abcd',
            //     maxZoom: 20,
            //     minZoom: 0
            // }).addTo(map);

            // Map drawing functions
            // get color depending on population density value
            function getBaseLog(x, y) {
                return Math.log(y) / Math.log(x);
            }

            // Add points
            var maxcasos = 0;
            var totalBR = 0;
            for (i = 0; i < casesData.length; i++) {
                maxcasos = Math.max(maxcasos, casesData[i]['total']);
                totalBR = totalBR + parseInt(casesData[i]['total']);
            }
            // control that shows state info on hover
            var info = L.control();
            info.onAdd = function (map) {
                this._div = L.DomUtil.create('div', 'info');
                this.update();
                return this._div;
            };
            info.update = function (props) {
                if (props) {
                    total = props.total;
                    cidade = props.name;
                    //console.log(props);
                    this._div.innerHTML = '<h4>Número de casos:</h4>' +  '<b>' + cidade + '</b><br />' + total; // + props.CODIGOINE;
                }
                else {
                    this._div.innerHTML =  '<h4>Total no Brasil:</h4><b>' + totalBR+'</b>';
                }
            };
            info.addTo(map);

            function highlightFeature(e) {
                info.update(e.sourceTarget.options);
            }
            function resetHighlight(e) {
                info.update();
            }



            var raioMin = 2;
            var raioMax = 10;
            function getRadius(total) {
                return 10000* ( (total - 1) / (maxcasos - 1) * (raioMax - raioMin) + 1 );
            }
            function getOpacity(tipo) {
                if (tipo == '0') {
                    return 0.2;
                }
                else {
                    return 0.8;
                }
            }
            function getColor(tipo) {
                if (tipo == '0') {
                    return 'orange';
                }
                else {
                    return 'red';
                }
            }

            markers = [];
            for (i = 0; i < casesData.length; i++) {
                markers.push(L.circle([casesData[i]['lat'], casesData[i]['lon']], {color: getColor(casesData[i]['type']), fillOpacity: getOpacity(casesData[i]['type']), radius: getRadius(casesData[i]['total']), name : casesData[i]['name'], total : casesData[i]['total']}).bindPopup(
                    '<b>'+casesData[i]['name']+'</b><br />Casos: '+casesData[i]['total']
                    ))
            }
            var pontos = L.featureGroup(markers).on({mouseover: highlightFeature,
                mouseout: resetHighlight}).addTo(map);

            // Legend control
            // var legend = L.control({position: 'bottomright'});
            // function buildLegend(map) {
            //         var div = L.DomUtil.create('div', 'info legend'),
            //         grades = riskd.legendGrades,
            //         labels = [],
            //         from, to;
            //     for (var i = 0; i < grades.length ; i++) {
            //         from = grades[i];
            //         labels.push(
            //             '<i style="background:' + getColor((from+from*1e-7)/100.0) + '"></i> ' +from+'%');
            //     }
            //     labels.push(
            //         '<i style="background: '+ colorSinDatos +'"></i> '+lang.NoData);

            //     div.innerHTML = labels.join('<br>');
            //     return div;
            // }
            // legend.onAdd = buildLegend;

            // Add extra plugins
            map.addControl(new L.Control.Fullscreen({position : 'bottomleft'})); // Fullscreen

            // Search box
            var searchControl = new L.Control.Search({
                layer: pontos,
                propertyName: 'name',
                marker: false,
                position : 'topleft',
                moveToLocation: function(latlng, title, map) {
                    //map.fitBounds( latlng.layer.getBounds() );
                    var zoom = map.getBoundsZoom(latlng.layer.getBounds());
                    map.setView(latlng, zoom); // access the zoom
                }
            });
            map.addControl(searchControl);

        };

        createMap();
    }
  
});