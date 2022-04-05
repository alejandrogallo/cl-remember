(in-package #:remember)

(flet ((reader (what)
         (lambda (entry) (getf (getf entry 'geolocation) what)))
       (writer (what) (lambda (value entry)
                        (unless #1=(getf entry 'geolocation)
                                (setf #1# nil))
                        (setf (getf #1# what) value))))
  (forms:defform geolocation (:name "Geolocation"
                              :id *form-name*
                              :method :post)
    ((latitude :string
               :placeholder "latitude"
               :label "Latitude"
               :writer (writer 'latitude)
               :reader (reader 'latitude))
     (longitude :string
                :placeholder "longitude"
                :label "Longitude"
                :writer (writer 'longitude)
                :reader (reader 'longitude))))

  (defmethod render-form (form (name (eql 'geolocation)))
    (who:with-html-output-to-string (forms.who:*html*)
      (:link :rel "stylesheet"
             :href "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"
             :crossorigin "")
      (:script :src "https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"
               :crossorigin "")
      (forms:with-form-theme 'forms.who:bootstrap-form-theme
        (forms:with-form-renderer :who
          (forms:render-form form)))

      ;; TODO
      #+(or)
      (:code
       (who:str
        (ps:ps
          (defun set-point (lat lng attrs map)
            (let ((circle (ps:chain *L
                                    (circle (list lat lng))
                                    (add-to map))))
              (ps:chain circle
                        (bind-popup (+ lat " - " lng))
                        (open-popup))))
          (let* ((main-lng "todo")
                 (main-lat "todo")
                 (map (ps:chain *L
                                (map "mapid")
                                (set-view (list main-lat main-lng) 8))))
            (ps:chain *L tile-layer
                      (ps:lisp (concatenate
                                'string
                                "https://api.mapbox.com/styles/v1/{id}/tiles/"
                                "{z}/{x}/{y}?access_token=pk.eyJ1IjoibWFwYm94"
                                "IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gif"
                                "Q.rJcFIG214AriISLbB6B5aw")))))))
      (:div :class "col-lg" :style "min-height: 500px;" :id "mapid")
      (who:str "<script>

                  function setPoint (lat, lng, attrs, map) {
                    const circle = L.circle([lat, lng], attrs).addTo(map);
                    circle.bindPopup(`${lat} - ${lng}`).openPopup();
                  }

                  const main_lng = {{entry.value.Geolocation.longitude}};
                  const main_lat = {{entry.value.Geolocation.latitude}};
                  var map = L.map('mapid')
                            .setView([ main_lat, main_lng ], 18);

                  L.tileLayer('https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gifQ.rJcFIG214AriISLbB6B5aw', {
                    maxZoom: 25,
                    attribution: 'Map data &copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors, ' +
                    'Imagery © <a href=\"https://www.mapbox.com/\">Mapbox</a>',
                    id: 'mapbox/streets-v11',
                    tileSize: 512,
                    zoomOffset: -1
                  }).addTo(map);

                  setPoint(main_lat,
                          main_lng,
                          { color: 'blue'
                          , radius: 1
                          , fillOpacity: 0.5
                          }
                          , map);


                  map.on(\"click\", (e) => {
                    const lat = e.latlng.lat;
                    const lng = e.latlng.lng;
                    console.log(e.latlng);
                    L.circle([lat, lng], {
                      color: 'red',
                      fillColor: '#f03',
                      fillOpacity: 0.5,
                      radius: 2
                    }).addTo(map).bindPopup(`${lat} - ${lng}`).openPopup();
                    document.getElementById(\"latitude\").value = lat;
                    document.getElementById(\"longitude\").value = lng;
                  });


                  map.locate({setView: false, watch: true})
                    .on('locationfound', function(e){
                      var marker = L.marker([e.latitude, e.longitude])
                                    .bindPopup('Your are here :)');
                      var you = L.circle([e.latitude, e.longitude], e.accuracy/2, {
                        weight: 1,
                        color: 'blue',
                        fillColor: '#cacaca',
                        fillOpacity: 0.2
                      })
                      map.addLayer(marker);
                      map.addLayer(you);
                      document.getElementById(\"latitude\").value = e.latitude;
                      document.getElementById(\"longitude\").value = e.longitude;
                    })
                    .on('locationerror', e => {
                      console.log(\"No possible locate you :(\")
                    });



                  </script>")

      )))
