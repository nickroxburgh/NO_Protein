extensions [ gis csv ]

globals [ year kommuner-list fylker-list farms-list slaughterhouse-list dairy-list checkpoint-list fgp-barley fgp-oats fgp-wheat fgp-rye-triticale fgp-oilseeds fgp-potatoes fgp-vegetables fgp-fodder-silage fgp-other-crops fgp-pome-stone-fruit fgp-berries fgp-other-cattle-carcass fgp-beef-cow-carcass fgp-dairy-cow-carcass fgp-raw-milk fgp-pig-carcass fgp-sheep-carcass fgp-broiler-carcass fgp-wool fgp-eggs dist-coeff total-imports-beef total-imports-pork total-imports-lamb total-imports-chicken total-imports-eggs total-imports-milk-cream total-imports-yoghurt total-imports-butter total-imports-cheese total-exports-beef total-exports-pork total-exports-lamb total-exports-chicken total-exports-eggs total-exports-milk-cream total-exports-yoghurt total-exports-butter total-exports-cheese production-per-capita-beef production-per-capita-pork production-per-capita-lamb production-per-capita-chicken production-per-capita-eggs production-per-capita-wool production-per-capita-rawmilk cm-cost-beef cm-cost-pork cm-cost-lamb cm-cost-chicken pf-cost-dairy pf-cost-eggs aggregate-production-cm-meat aggregate-production-pf-dairy aggregate-production-pf-eggs emissions-ha-barley emissions-ha-oats emissions-ha-wheat emissions-ha-rye-triticale emissions-ha-oilseeds emissions-ha-potatoes emissions-ha-vegetables emissions-ha-fodder-silage emissions-ha-other-crops emissions-ha-orchards emissions-ha-berries emissions-head-dairy-cows emissions-head-beef-cows emissions-head-other-cattle emissions-head-sheep emissions-head-pigs emissions-head-broilers emissions-head-laying-hens ]

breed [ kommuner kommune ]
breed [ fylker fylke ]
breed [ farms farm ]
breed [ slaughterhouses slaughterhouse ]
breed [ dairies dairy ]
breed [ checkpoints checkpoint ]
breed [ cm-factories cm-factory ]
breed [ pf-factories pf-factory ]
directed-link-breed [ farm-dairy-links farm-dairy-link ]
directed-link-breed [ farm-slaughterhouse-links farm-slaughterhouse-link ]

patches-own [ parent-kommune ]
kommuner-own [ kommune-id fylke-id kommune-name fylke-name population consumption-beef consumption-pork consumption-lamb consumption-chicken consumption-eggs consumption-milk-cream consumption-yoghurt consumption-butter consumption-cheese ]
fylker-own [ fylke-id yield-barley yield-oats yield-wheat yield-rye-triticale yield-oilseeds yield-potatoes yield-vegetables yield-fodder-silage yield-other-crops yield-orchards yield-berries yield-other-cattle-carcass yield-beef-cow-carcass yield-dairy-cow-carcass yield-raw-milk yield-pig-carcass yield-sheep-carcass yield-broiler-carcass yield-wool yield-eggs ]
farms-own [ farm-id active? kommune-id fylke-id partner-dairy dairy-distance-km partner-slaughterhouse slaughterhouse-distance-km ha-barley ha-oats ha-wheat ha-rye-triticale ha-oilseeds ha-potatoes ha-vegetables ha-fodder-silage ha-other-crops ha-orchards ha-berries num-dairy-cows num-beef-cows num-other-cattle num-sheep num-pigs num-broilers num-laying-hens subsidy-nonlivestock subsidy-livestock subsidy-milk benchmark-income annual-income recent-income farm-emissions ]
slaughterhouses-own [ slaughterhouse-name min-viable-carcass supply-beef supply-pork supply-lamb supply-chicken supply-eggs processed-beef processed-pork processed-lamb processed-chicken processed-eggs wholesale-stock-beef wholesale-stock-pork wholesale-stock-lamb wholesale-stock-chicken wholesale-stock-eggs active? ]
dairies-own[ dairy-name min-viable-rawmilk supply-rawmilk processed-rawmilk wholesale-stock-milk-cream wholesale-stock-yoghurt wholesale-stock-butter wholesale-stock-cheese active? ]
checkpoints-own [ checkpoint-id checkpoint-name checkpoint-type destination imports-beef imports-pork imports-lamb imports-chicken imports-milk-cream imports-yoghurt imports-butter imports-cheese imports-eggs exports-beef exports-pork exports-lamb exports-chicken exports-milk-cream exports-yoghurt exports-butter exports-cheese exports-eggs ]
cm-factories-own [ meat-type wholesale-stock processed-meat factory-emissions ]
pf-factories-own [ product-type wholesale-stock-milk-cream wholesale-stock-yoghurt wholesale-stock-butter wholesale-stock-cheese wholesale-stock-eggs processed-rawmilk processed-eggs factory-emissions ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP                                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  ca
  clear-all
  file-close-all
  set year start-yr
  if param-scenario = "Default" [
    set-params-to-default
  ]
  setup-maps
  setup-kommuner
  setup-fylker
  setup-population
  setup-farms
  setup-slaughterhouses
  setup-dairies
  setup-crop-yields
  setup-animal-yields
  setup-farm-gate-price
  calibrate-distance
  setup-farm-to-processor-links
  setup-checkpoints
  setup-consumption
  setup-trade-records
  setup-per-capita-production-records
  setup-cultured-meat
  setup-biosynthetic-liquid
  setup-emissions
  reset-ticks
end

to setup-maps
  file-close-all
  ; Setup world aesthetics.
  ask patches [ set pcolor white ]
  ; Load the geographical datasets using the approach set out in Wilensky & Rand (2015, p.404-404).
  set kommuner-list gis:load-dataset "Data/NO_AdministrativeBorders/ProcessedData/NO_NetLogoKommuner.geojson"
  set fylker-list gis:load-dataset "Data/NO_AdministrativeBorders/ProcessedData/NO_NetLogoFylker.geojson"
  set slaughterhouse-list gis:load-dataset "Data/NO_Slaughterhouses/ProcessedData/NO_NetLogoSlaughterhouses.geojson"
  set dairy-list gis:load-dataset "Data/NO_Dairies/ProcessedData/NO_NetLogoDairies.geojson"
  if start-yr = 2013 [ set farms-list gis:load-dataset "Data/NO_Farms/ProcessedData/NO_NetLogoFarms2013.geojson" ]
  if start-yr = 2020 [ set farms-list gis:load-dataset "Data/NO_Farms/ProcessedData/NO_NetLogoFarms2020.geojson" ]
  set checkpoint-list gis:load-dataset "Data/NO_BorderCheckpoints/ProcessedData/NO_NetLogoBorderCheckpoints.geojson"
  ; Set the world envelope to the union of all of our dataset's envelopes.
  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of kommuner-list) (gis:envelope-of fylker-list) (gis:envelope-of farms-list) (gis:envelope-of slaughterhouse-list) (gis:envelope-of dairy-list) (gis:envelope-of checkpoint-list))
  ; Draw the kommuner and associate patches with the admin units they lie within.
  gis:set-drawing-color 8
  gis:draw kommuner-list 0.5
  ; Draw fylker and set the fill to grey.
  gis:set-drawing-color 7
  gis:draw fylker-list 1
  ask patches gis:intersecting fylker-list [ set pcolor 9 ]
  file-close
end

to setup-kommuner
  file-close-all
  ; One at a time, add kommuner as agents.
  foreach gis:feature-list-of kommuner-list [ current-kommune ->
    let centroid gis:location-of gis:centroid-of current-kommune
    create-kommuner 1  [
      ; Set the kommuner ID and its associated fylker ID.
      set kommune-id gis:property-value current-kommune "KommunerID"
      set fylke-id gis:property-value current-kommune "FylkerID"
      set kommune-name gis:property-value current-kommune "KommunerName"
      set fylke-name gis:property-value current-kommune "FylkerName"
      set population []
      set consumption-beef []
      set consumption-pork []
      set consumption-lamb []
      set consumption-chicken []
      set consumption-eggs []
      set consumption-milk-cream []
      set consumption-yoghurt []
      set consumption-butter []
      set consumption-cheese []
      ; Specify aesthetics.
      set xcor item 0 centroid
      set ycor item 1 centroid
      set color 6
      set shape "circle"
      set size 3
      if hide-kommuner? = TRUE [
        set hidden? TRUE
      ]
    ]
  ]
  ; When reading in CSV files, NetLogo automatically assumes the kommune ID's in the CSV are numeric
  ; rather than character so converts  "0301" to "301". This makes it difficult to match data to the
  ; kommune agent for this kommune. We'll resolve this by changing the agents kommune ID to the
  ; short version. This can be reversed at the end of the setup if now more matching is needed.
  ask kommuner with [ kommune-id = "0301" ] [ set kommune-id "301" ]
  file-close
end

to setup-fylker
  file-close-all
  ; One at a time, add fylker as agents.
  foreach gis:feature-list-of fylker-list [ current-fylke ->
    let centroid gis:location-of gis:centroid-of current-fylke
    create-fylker 1  [
      ; Set the fylke ID.
      set fylke-id gis:property-value current-fylke "FylkerID"
      ; Initialise crop yield lists.
      set yield-barley []
      set yield-oats []
      set yield-wheat []
      set yield-rye-triticale []
      set yield-oilseeds []
      set yield-potatoes []
      set yield-vegetables []
      set yield-fodder-silage []
      set yield-other-crops []
      set yield-orchards []
      set yield-berries []
      set yield-other-cattle-carcass []
      set yield-beef-cow-carcass []
      set yield-dairy-cow-carcass []
      set yield-raw-milk []
      set yield-pig-carcass []
      set yield-sheep-carcass []
      set yield-broiler-carcass []
      set yield-wool []
      set yield-eggs []
      ; Specify aesthetics.
      set xcor item 0 centroid
      set ycor item 1 centroid
      set color 6
      set shape "triangle"
      set size 10
      if hide-fylker? = TRUE [
        set hidden? TRUE
      ]
    ]
  ]
  ; When reading in CSV files, NetLogo automatically assumes the kommune ID's in the CSV are numeric
  ; rather than character so converts  "0301" to "301". This makes it difficult to match data to the
  ; kommune agent for this kommune. We'll resolve this by changing the agents kommune ID to the
  ; short version. This can be reversed at the end of the setup if now more matching is needed.
  ask kommuner with [ kommune-id = "0301" ] [ set kommune-id "301" ]
  file-close
  file-close
end

to setup-population
  file-close-all
  ; Begin by adding the observed population data.
  file-open "Data/NO_Population/ProcessedData/NO_Population.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-kommune item 0 row
    let current-year item 1 row
    let current-population item 2 row

    ; Drop the first sublist as it contains the header rather than any data of relevance & ignore data that
    ; preceeds the simulation start year.
    if current-year != "Year" and current-year >= start-yr [
      ; Identify the kommune that matches the current-kommune ID, and ask it to add the current-year
      ; and current-population as a sublist to the end of its population list.
      ask kommuner with [(word kommune-id) = (word current-kommune)] [
        set population lput list ( current-year ) ( current-population ) population
      ]
    ]

  ]
  file-close
  ; Next, add the projected population data, choosing the scenario specified by the interface chooser.
  file-open "Data/NO_Population/ProcessedData/NO_PopulationProj.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-kommune item 0 row
    let current-year item 1 row
    let current-scenario item 2 row
    let current-population item 3 row
    ; We are only interested in data applicable to the chosen scenario.
    if current-scenario = population-growth [
      ; Identify the kommune that matches the current-kommune ID, and ask it to add the current-year
      ; and current-population as a sublist to the end of its population list.
      ask kommuner with [(word kommune-id) = (word current-kommune)] [
        set population lput list ( current-year ) ( current-population ) population
      ]
    ]
  ]
  file-close
end

to setup-farms
  file-close-all
  ; One at a time, add ports as agents.
  foreach n-of num-farms-to-sim gis:feature-list-of farms-list [ current-farm ->
    let centroid gis:location-of gis:centroid-of current-farm
    create-farms 1 [
      ; Assign each farm their ID, kommune ID, and details of their activities, plus initialise lists
      ; that will be used to determine the benchmark income they aspire to and their annual income.
      set farm-id gis:property-value current-farm "FarmerID"
      set active? TRUE
      set kommune-id gis:property-value current-farm "KommunerID"
      set fylke-id gis:property-value current-farm "FylkerID"
      set ha-barley gis:property-value current-farm "Barley"
      set ha-oats gis:property-value current-farm "Oats"
      set ha-wheat gis:property-value current-farm "Wheat"
      set ha-rye-triticale gis:property-value current-farm "Rye & triticale"
      set ha-oilseeds gis:property-value current-farm "Oilseeds"
      set ha-potatoes gis:property-value current-farm "Potatoes"
      set ha-vegetables gis:property-value current-farm "Vegetables"
      set ha-fodder-silage gis:property-value current-farm "Green fodder & silage"
      set ha-other-crops gis:property-value current-farm "Other crops"
      set ha-orchards gis:property-value current-farm "Orchards"
      set ha-berries gis:property-value current-farm "Berries"
      set num-dairy-cows gis:property-value current-farm "Dairy cows"
      set num-beef-cows gis:property-value current-farm "Beef cows"
      set num-other-cattle gis:property-value current-farm "Other cattle"
      set num-sheep gis:property-value current-farm "Sheep"
      set num-pigs gis:property-value current-farm "Pigs"
      set num-broilers gis:property-value current-farm "Broilers"
      set num-laying-hens gis:property-value current-farm "Laying hens"
      set subsidy-nonlivestock gis:property-value current-farm "Non-livestock subsidy"
      set subsidy-livestock gis:property-value current-farm "Livestock subsidy"
      set subsidy-milk gis:property-value current-farm "Milk subsidy"
      set benchmark-income []
      set annual-income []
      set recent-income []
      ; Specify aesthetics.
      set xcor item 0 centroid
      set ycor item 1 centroid
      set color [77 123 106 125]
      set shape "circle"
      set size 1
      if hide-farms? = TRUE [
        set hidden? TRUE
      ]
    ]
  ]
  file-close
end

to setup-slaughterhouses
  file-close-all
  ; One at a time, add slaughterhouses as agents.
  foreach gis:feature-list-of slaughterhouse-list [ current-slaughterhouse ->
    let centroid gis:location-of gis:centroid-of current-slaughterhouse
    create-slaughterhouses 1  [
      ; Set the name and the processing capacity. The max processing capacity is set for each animal
      ; type individually. It is the number of slaughters performed in 2020 by the slaughterhouse as
      ; reported by Animalia (2021, p.132), plus a percentage buffer specified by the slaughter-max-
      ; capacity slider. The min-viable-carcass threshold is the point at which the slaughterhouse is
      ; no longer considered viable. It is the total carcass weight processed in 2020, multiplied by
      ; a slaughter-min-capacity slider value. The wholesale-meat lists are records of the amount of
      ; meat the slaughterhouse have processed in each given year.
      set slaughterhouse-name gis:property-value current-slaughterhouse "Slaughterhouse"
      set min-viable-carcass 0
      set supply-beef 0
      set supply-pork 0
      set supply-lamb 0
      set supply-chicken 0
      set supply-eggs 0
      set processed-beef []
      set processed-pork []
      set processed-lamb []
      set processed-chicken []
      set processed-eggs []
      set wholesale-stock-beef 0
      set wholesale-stock-pork 0
      set wholesale-stock-lamb 0
      set wholesale-stock-chicken 0
      set wholesale-stock-eggs 0
      set active? TRUE
      ; Specify aesthetics.
      set xcor item 0 centroid
      set ycor item 1 centroid
      set color black
      set shape "square"
      set size 5
      if hide-slaughterhouses? = TRUE [
        set hidden? TRUE
      ]
    ]
  ]
  file-close
end

to setup-dairies
  file-close-all
  ; One at a time, add dairies as agents.
  foreach gis:feature-list-of dairy-list [ current-dairy ->
    let centroid gis:location-of gis:centroid-of current-dairy
    create-dairies 1  [
      ; Set the name and initialise variables that will be calibrated during the first step to
      ; specify processing capacity and raw milk processing threshold below which the dairy will
      ; no longer be considered viable.
      set dairy-name gis:property-value current-dairy "Dairy"
      set min-viable-rawmilk 0
      set supply-rawmilk 0
      set processed-rawmilk []
      set wholesale-stock-milk-cream 0
      set wholesale-stock-yoghurt 0
      set wholesale-stock-butter 0
      set wholesale-stock-cheese 0
      set active? TRUE
      ; Specify aesthetics.
      set xcor item 0 centroid
      set ycor item 1 centroid
      set color black
      set shape "circle"
      set size 5
      if hide-dairies? = TRUE [
        set hidden? TRUE
      ]
    ]
  ]
  file-close
end

to setup-crop-yields
  file-close-all
  file-open "Data/NO_Yields/ProcessedData/NO_NetLogoCropYield.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-year item 0 row
    ; Drop the first sublist as it contains the header rather than any data of relevance & ignore data that
    ; preceeds the simulation start year.
    if current-year != "Year" and current-year >= start-yr [
      let current-fylker item 1 row
      ; Identify the fylke that matches the current-fylker ID.
      ask fylker with [(word fylke-id) = (word current-fylker)] [
        ; Ask this fylke to generate a yield value for the current crop based on the specified yield, plus a
        ;randomly determined noise value, then add this value to the end of the nested list for the crop of interest.
        let current-yield random-normal item 4 row item 5 row
        if item 2 row = "Barley" [ set yield-barley lput list (current-year) (current-yield) yield-barley ]
        if item 2 row = "Oats" [ set yield-oats lput list (current-year) (current-yield) yield-oats ]
        if item 2 row = "Wheat" [ set yield-wheat lput list (current-year) (current-yield) yield-wheat ]
        if item 2 row = "Rye & triticale" [ set yield-rye-triticale lput list (current-year) (current-yield) yield-rye-triticale ]
        if item 2 row = "Oilseeds" [ set yield-oilseeds lput list (current-year) (current-yield) yield-oilseeds ]
        if item 2 row = "Potatoes" [ set yield-potatoes lput list (current-year) (current-yield) yield-potatoes ]
        if item 2 row = "Vegetables" [ set yield-vegetables lput list (current-year) (current-yield) yield-vegetables ]
        if item 2 row = "Green fodder & silage" [ set yield-fodder-silage lput list (current-year) (current-yield) yield-fodder-silage ]
        if item 2 row = "Other crops" [ set yield-other-crops lput list (current-year) (current-yield) yield-other-crops ]
        if item 2 row = "Orchards" [ set yield-orchards lput list (current-year) (current-yield) yield-orchards ]
        if item 2 row = "Berries" [ set yield-berries lput list (current-year) (current-yield) yield-berries ]
      ]
    ]
  ]
  file-close
end

to setup-animal-yields
  file-close-all
  file-open "Data/NO_Yields/ProcessedData/NO_NetLogoAnimalYield.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-year item 0 row
    ; Drop the first sublist as it contains the header rather than any data of relevance & ignore data that
    ; preceeds the simulation start year.
    if current-year != "Year" and current-year >= start-yr [
      let current-fylker item 1 row
      ; Identify the fylker that matches the current-fylker ID.
      ask fylker with [(word fylke-id) = (word current-fylker)] [
        ; The yield selected depends on whether the user wishes to use constant yield or trend yield trajectories post-2020.
        let current-yield 0
        if animal-yield-trajectory = "Constant" [ set current-yield item 4 row ]
        if animal-yield-trajectory = "Trend" [ set current-yield item 5 row ]
        ; Ask the fylke to add the yield value to the end of the nested list for the raw agricultural product of interest.
        if item 3 row = "Other cattle carcass" [ set yield-other-cattle-carcass lput list (current-year) (current-yield) yield-other-cattle-carcass ]
        if item 3 row = "Beef cow carcass" [ set yield-beef-cow-carcass lput list (current-year) (current-yield) yield-beef-cow-carcass ]
        if item 3 row = "Dairy cow carcass" [ set yield-dairy-cow-carcass lput list (current-year) (current-yield) yield-dairy-cow-carcass ]
        if item 3 row = "Raw milk" [ set yield-raw-milk lput list (current-year) (current-yield) yield-raw-milk ]
        if item 3 row = "Pig carcass" [ set yield-pig-carcass lput list (current-year) (current-yield) yield-pig-carcass ]
        if item 3 row = "Sheep carcass" [ set yield-sheep-carcass lput list (current-year) (current-yield) yield-sheep-carcass ]
        if item 3 row = "Broiler carcass" [ set yield-broiler-carcass lput list (current-year) (current-yield) yield-broiler-carcass ]
        if item 3 row = "Wool" [ set yield-wool lput list (current-year) (current-yield) yield-wool ]
        if item 3 row = "Eggs" [ set yield-eggs lput list (current-year) (current-yield) yield-eggs ]
      ]
    ]
  ]
  file-close
end

to setup-farm-gate-price
  ; Initialise farm gate price lists.
  set fgp-barley []
  set fgp-oats []
  set fgp-wheat []
  set fgp-rye-triticale []
  set fgp-oilseeds []
  set fgp-potatoes []
  set fgp-vegetables []
  set fgp-fodder-silage []
  set fgp-other-crops []
  set fgp-pome-stone-fruit []
  set fgp-berries []
  set fgp-other-cattle-carcass []
  set fgp-beef-cow-carcass []
  set fgp-dairy-cow-carcass []
  set fgp-raw-milk []
  set fgp-pig-carcass []
  set fgp-sheep-carcass []
  set fgp-broiler-carcass []
  set fgp-wool []
  set fgp-eggs []
  ; Read in price data to populate lists.
  file-close-all
  file-open "Data/NO_ProducerPrice/ProcessedData/NO_NetLogoProducerPrice.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-year item 0 row
    let current-price item 2 row
    ; Drop the first sublist as it contains the header rather than any data of relevance & ignore data that
    ; preceeds the simulation start year.
    if current-year != "Year" and current-year >= start-yr [
      if item 1 row = "Barley" [ set fgp-barley lput list (current-year) (current-price) fgp-barley ]
      if item 1 row = "Oats" [ set fgp-oats lput list (current-year) (current-price) fgp-oats ]
      if item 1 row = "Wheat" [ set fgp-wheat lput list (current-year) (current-price) fgp-wheat ]
      if item 1 row = "Rye & triticale" [ set fgp-rye-triticale lput list (current-year) (current-price) fgp-rye-triticale ]
      if item 1 row = "Oilseeds" [ set fgp-oilseeds lput list (current-year) (current-price) fgp-oilseeds ]
      if item 1 row = "Potatoes" [ set fgp-potatoes lput list (current-year) (current-price) fgp-potatoes ]
      if item 1 row = "Vegetables" [ set fgp-vegetables lput list (current-year) (current-price) fgp-vegetables ]
      if item 1 row = "Green fodder & silage" [ set fgp-fodder-silage lput list (current-year) (current-price) fgp-fodder-silage ]
      if item 1 row = "Other crops" [ set fgp-other-crops lput list (current-year) (current-price) fgp-other-crops ]
      if item 1 row = "Pome & stone fruits" [ set fgp-pome-stone-fruit lput list (current-year) (current-price) fgp-pome-stone-fruit ]
      if item 1 row = "Berries" [ set fgp-berries lput list (current-year) (current-price) fgp-berries ]
      ; The post-2020 livestock product prices are set seperately so we cease reading them in after 2020.
      if current-year <= 2020 [
        if item 1 row = "Other cattle carcass" [ set fgp-other-cattle-carcass lput list (current-year) (current-price) fgp-other-cattle-carcass ]
        if item 1 row = "Beef cow carcass" [ set fgp-beef-cow-carcass lput list (current-year) (current-price) fgp-beef-cow-carcass ]
        if item 1 row = "Dairy cow carcass" [ set fgp-dairy-cow-carcass lput list (current-year) (current-price) fgp-dairy-cow-carcass ]
        if item 1 row = "Raw milk" [ set fgp-raw-milk lput list (current-year) (current-price) fgp-raw-milk ]
        if item 1 row = "Pig carcass" [ set fgp-pig-carcass lput list (current-year) (current-price) fgp-pig-carcass ]
        if item 1 row = "Sheep carcass" [ set fgp-sheep-carcass lput list (current-year) (current-price) fgp-sheep-carcass ]
        if item 1 row = "Broiler carcass" [ set fgp-broiler-carcass lput list (current-year) (current-price) fgp-broiler-carcass ]
        if item 1 row = "Wool" [ set fgp-wool lput list (current-year) (current-price) fgp-wool ]
        if item 1 row = "Eggs" [ set fgp-eggs lput list (current-year) (current-price) fgp-eggs ]
      ]
    ]
  ]
  file-close
  ; If the price scenario selected calls for prices to be set by users...
  if price-scenario = "User specified" [
    ; The empirical data used to inform the farm-gate price scenarios only takes us to 2020 so we need
    ; to determine values for subsequent years. We do this by assuming constant year-on-year growth
    ; with the rate of growth determined by the `price-growth-product` sliders. It can be between
    ; -5% and 5%.
    let current-year 2021
    loop [
      if current-year > sim-end-yr [ stop ]
      ; Determine the previous year's beef price (which is the same for all cattle types), then calculate
      ; what the current price should be given the slider specified growth, then add it to the fgp lists
      ; of the various cattle types.
      let previous-fgp filter [ i -> (current-year - 1) = item 0 i ] fgp-other-cattle-carcass
      set previous-fgp item 1 item 0 previous-fgp
      let current-fgp (previous-fgp + (previous-fgp * (price-growth-beef / 100)))
      set fgp-other-cattle-carcass lput list (current-year) (current-fgp) fgp-other-cattle-carcass
      set fgp-dairy-cow-carcass lput list (current-year) (current-fgp) fgp-dairy-cow-carcass
      set fgp-beef-cow-carcass lput list (current-year) (current-fgp) fgp-beef-cow-carcass
      ; Repeat for pork
      set previous-fgp filter [ i -> (current-year - 1) = item 0 i ] fgp-pig-carcass
      set previous-fgp item 1 item 0 previous-fgp
      set current-fgp (previous-fgp + (previous-fgp * (price-growth-pork / 100)))
      set fgp-pig-carcass lput list (current-year) (current-fgp) fgp-pig-carcass
      ; Repeat for lamb
      set previous-fgp filter [ i -> (current-year - 1) = item 0 i ] fgp-sheep-carcass
      set previous-fgp item 1 item 0 previous-fgp
      set current-fgp (previous-fgp + (previous-fgp * (price-growth-lamb / 100)))
      set fgp-sheep-carcass lput list (current-year) (current-fgp) fgp-sheep-carcass
      ; Repeat for chicken
      set previous-fgp filter [ i -> (current-year - 1) = item 0 i ] fgp-broiler-carcass
      set previous-fgp item 1 item 0 previous-fgp
      set current-fgp (previous-fgp + (previous-fgp * (price-growth-chicken / 100)))
      set fgp-broiler-carcass lput list (current-year) (current-fgp) fgp-broiler-carcass
      ; Repeat for eggs
      set previous-fgp filter [ i -> (current-year - 1) = item 0 i ] fgp-eggs
      set previous-fgp item 1 item 0 previous-fgp
      set current-fgp (previous-fgp + (previous-fgp * (price-growth-eggs / 100)))
      set fgp-eggs lput list (current-year) (current-fgp) fgp-eggs
      ; Repeat for raw milk
      set previous-fgp filter [ i -> (current-year - 1) = item 0 i ] fgp-raw-milk
      set previous-fgp item 1 item 0 previous-fgp
      set current-fgp (previous-fgp + (previous-fgp * (price-growth-raw-milk / 100)))
      set fgp-raw-milk lput list (current-year) (current-fgp) fgp-raw-milk
      ; Repeat for wool
      set previous-fgp filter [ i -> (current-year - 1) = item 0 i ] fgp-wool
      set previous-fgp item 1 item 0 previous-fgp
      set current-fgp (previous-fgp + (previous-fgp * (price-growth-wool / 100)))
      set fgp-wool lput list (current-year) (current-fgp) fgp-wool
      set current-year current-year + 1
    ]
  ]
  ; If the prices are determined by markets, they will be updated during the go procedure from 2021 onwards.
end

to calibrate-distance
  ; We need to determine roughly how NetLogo distances translate into real world distances. We can do
  ; this by calculating the NetLogo world distance between various pairs of agents, and comparing this
  ; to the known real world distances between those pairs of agents in km. We'll average the conversion
  ; coefficient of five modestly seperated dairies. For converting modest distances, this method appears
  ; pretty good, but performance drops when travelling more than a few hundred km. We'll use this
  ; coefficient in a reporter than will return a distance value in km for specified pairs of agents.
  let actual-distance-a 186.48
  let actual-distance-b 352
  let actual-distance-c 188.6
  let actual-distance-d 161.4
  let actual-distance-e 68.95
  let netlogo-distance-a [ distance one-of dairies with [ dairy-name = "TINE Meieriet Tana" ] ] of one-of dairies with [ dairy-name = "TINE Meieriet Alta" ]
  let netlogo-distance-b [ distance one-of dairies with [ dairy-name = "TINE Meieriet Sandnessjøen" ] ] of one-of dairies with [ dairy-name = "TINE Meieriet Harstad" ]
  let netlogo-distance-c [ distance one-of dairies with [ dairy-name = "TINE Meieriet Sem" ] ] of one-of dairies with [ dairy-name = "Hennig-Olsen Iskremfabrikk" ]
  let netlogo-distance-d [ distance one-of dairies with [ dairy-name = "TINE Meieriet Sem" ] ] of one-of dairies with [ dairy-name = "TINE Meieriet Setesdal" ]
  let netlogo-distance-e [ distance one-of dairies with [ dairy-name = "Synnøve Finden Alvdal" ] ] of one-of dairies with [ dairy-name = "TINE Meieriet Frya" ]
  let coeff-a actual-distance-a / netlogo-distance-a
  let coeff-b actual-distance-b / netlogo-distance-b
  let coeff-c actual-distance-c / netlogo-distance-c
  let coeff-d actual-distance-d / netlogo-distance-d
  let coeff-e actual-distance-e / netlogo-distance-e
  set dist-coeff (coeff-a + coeff-b + coeff-c + coeff-d + coeff-e) / 5
end

to setup-farm-to-processor-links
  ; Dairies.
  ; Ask each farm with dairy cows to identify their nearest dairy as the crow flies. This dairy becomes
  ; its initial partner dairy. We also calculate the distance.
  ask farms with [ num-dairy-cows > 0 ] [
    set partner-dairy min-one-of dairies [ distance myself ]
    create-farm-dairy-link-to partner-dairy [
      tie
      set color [239 213 167 125]
      if hide-farm-dairy-links? = TRUE [ set hidden? TRUE ]
    ]
    set dairy-distance-km (item 0 [link-length] of my-farm-dairy-links) * dist-coeff
  ]
  ; Initialise the dairy min and max raw milk processing capacity by calculating initial milk
  ; production of partner farms and applying the buffers specified by dairy-min-capacity and
  ; dairy-max-capacity.
  ask dairies [
    let raw-milk-supply-initial 0
    ask my-farm-dairy-links [ ask other-end [ set raw-milk-supply-initial raw-milk-supply-initial + farm-production-raw-milk ] ]
    set min-viable-rawmilk (raw-milk-supply-initial / 100) * dairy-min-capacity
  ]
  ; Slaughterhouses.
  ; Ask each farm with livestock to identify their nearest slaughterhouse as the crow flies. This
  ; slaughterhouse becomes its initial partner slaughterhouse. We also calculate the distance.
  ask farms with [ (num-dairy-cows + num-beef-cows + num-other-cattle + num-pigs + num-sheep + num-broilers + num-laying-hens ) > 0 ] [
    set partner-slaughterhouse min-one-of slaughterhouses [ distance myself ]
    create-farm-slaughterhouse-link-to partner-slaughterhouse [
      tie
      set color [204 116 94 125]
      if hide-farm-slaught-links? = TRUE [ set hidden? TRUE ]
    ]
    set slaughterhouse-distance-km (item 0 [link-length] of my-farm-slaughterhouse-links) * dist-coeff
  ]
  ; Initialise the slaughterhouse min and max carcass processing capacity by calculating initial
  ; carcass production of partner farms and applying the buffers specified by slaughter-min-capacity and
  ; slaughter-max-capacity.
  ask slaughterhouses [
    let carcass-supply-initial 0
    ask my-farm-slaughterhouse-links [ ask other-end [ set carcass-supply-initial carcass-supply-initial + (farm-production-beef + farm-production-lamb + farm-production-pork + farm-production-chicken) ] ]
    set min-viable-carcass (carcass-supply-initial / 100) * slaughter-min-capacity
  ]
end

to setup-checkpoints
  file-close-all
  ; One at a time, add ports as agents.
  foreach gis:feature-list-of checkpoint-list [ current-checkpoint ->
    let centroid gis:location-of gis:centroid-of current-checkpoint
    create-checkpoints 1 [
      ; Set the ID and name.
      set checkpoint-id gis:property-value current-checkpoint "CheckpointID"
      set checkpoint-name gis:property-value current-checkpoint "CheckpointName"
      set checkpoint-type gis:property-value current-checkpoint "CheckpointType"
      set destination gis:property-value current-checkpoint "Destination"
      set imports-beef 0
      set imports-pork 0
      set imports-lamb 0
      set imports-chicken 0
      set imports-milk-cream 0
      set imports-yoghurt 0
      set imports-butter 0
      set imports-cheese 0
      set imports-eggs 0
      set exports-beef 0
      set exports-pork 0
      set exports-lamb 0
      set exports-chicken 0
      set exports-milk-cream 0
      set exports-yoghurt 0
      set exports-butter 0
      set exports-cheese 0
      set exports-eggs 0
      ; Specify aesthetics.
      set xcor item 0 centroid
      set ycor item 1 centroid
      set color black
      set shape "triangle"
      set size 7
      if hide-checkpoints? = TRUE [
        set hidden? TRUE
      ]
    ]
  ]
  ; We'll also set up globals to record total imports and exports.
;  set total-imports-beef []
;  set total-exports-beef []
  file-close
end

to setup-consumption
  file-close-all
  file-open "Data/NO_FoodConsumption/ProcessedData/NO_ConsumptionWholesale.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-year item 1 row
    let current-product item 0 row
    let current-consumption item 2 row
    ; Drop the first sublist as it contains the header rather than any data of relevance & ignore data that
    ; preceeds the simulation start year.
    if current-year != "Year" and current-year >= start-yr [
      ask kommuner [
        ; Extract the kommune population value for the relevant year.
        let current-year-population filter [ i -> current-year = item 0 i ] population
        set current-year-population item 1 item 0 current-year-population
        ; Populate the relevant consumption list with the per capita consumption value (i.e., current-consumption)
        ; multiplied by the population of the kommune in that year (i.e., current-year-population), converted to tonnes.
        if current-product = "Beef" [ set consumption-beef lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-beef ]
        if current-product = "Pork" [ set consumption-pork lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-pork ]
        if current-product = "Lamb" [ set consumption-lamb lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-lamb ]
        if current-product = "Chicken" [ set consumption-chicken lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-chicken ]
        if current-product = "Eggs" [ set consumption-eggs lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-eggs ]
        if current-product = "Milk & cream" [ set consumption-milk-cream lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-milk-cream ]
        if current-product = "Yoghurt" [ set consumption-yoghurt lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-yoghurt ]
        if current-product = "Butter" [ set consumption-butter lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-butter ]
        if current-product = "Cheese" [ set consumption-cheese lput list (current-year) ((current-consumption * current-year-population) / 1000) consumption-cheese ]
      ]
    ]
  ]
  file-close
  ; The empirical data use to inform the consumption scenarios only takes us to 2020 so we need
  ; to determine values for subsequent years. We do this by assuming constant year-on-year growth
  ; with the rate of growth determined by the `consum-growth-product` sliders. It can be between
  ; -5% and 5%.
  ask kommuner [
    let current-year 2021
    loop [
      if current-year > sim-end-yr [ stop ]
      ; Extract the kommune population value for the previous year.
      let previous-year-population filter [ i -> (current-year - 1) = item 0 i ] population
      set previous-year-population item 1 item 0 previous-year-population
      ; Extract the kommune population value for the current year.
      let current-year-population filter [ i -> current-year = item 0 i ] population
      set current-year-population item 1 item 0 current-year-population
      ; Determine the previous year's per capita beef consumption, then calculate what the current consumption should be given growth.
      let previous-consumption last last consumption-beef / previous-year-population
      let current-consumption (previous-consumption + (previous-consumption * (consum-growth-beef / 100))) * current-year-population
      set consumption-beef lput list (current-year) (current-consumption) consumption-beef
      ; Repeat for pork.
      set previous-consumption last last consumption-pork / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-pork / 100))) * current-year-population
      set consumption-pork lput list (current-year) (current-consumption) consumption-pork
      ; Repeat for lamb.
      set previous-consumption last last consumption-lamb / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-lamb / 100))) * current-year-population
      set consumption-lamb lput list (current-year) (current-consumption) consumption-lamb
      ; Repeat for chicken.
      set previous-consumption last last consumption-chicken / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-chicken / 100))) * current-year-population
      set consumption-chicken lput list (current-year) (current-consumption) consumption-chicken
      ; Repeat for eggs.
      set previous-consumption last last consumption-eggs / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-eggs / 100))) * current-year-population
      set consumption-eggs lput list (current-year) (current-consumption) consumption-eggs
      ; Repeat for milk and cream.
      set previous-consumption last last consumption-milk-cream / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-milk-cream / 100))) * current-year-population
      set consumption-milk-cream lput list (current-year) (current-consumption) consumption-milk-cream
      ; Repeat for yoghurt.
      set previous-consumption last last consumption-yoghurt / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-yoghurt / 100))) * current-year-population
      set consumption-yoghurt lput list (current-year) (current-consumption) consumption-yoghurt
      ; Repeat for butter.
      set previous-consumption last last consumption-butter / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-butter / 100))) * current-year-population
      set consumption-butter lput list (current-year) (current-consumption) consumption-butter
      ; Repeat for Cheese.
      set previous-consumption last last consumption-cheese / previous-year-population
      set current-consumption (previous-consumption + (previous-consumption * (consum-growth-cheese / 100))) * current-year-population
      set consumption-cheese lput list (current-year) (current-consumption) consumption-cheese
      set current-year current-year + 1
    ]
  ]
end

to setup-trade-records
  set total-imports-beef []
  set total-imports-pork []
  set total-imports-lamb []
  set total-imports-chicken []
  set total-imports-eggs []
  set total-imports-milk-cream []
  set total-imports-yoghurt []
  set total-imports-butter []
  set total-imports-cheese []
  set total-exports-beef []
  set total-exports-pork []
  set total-exports-lamb []
  set total-exports-chicken []
  set total-exports-eggs []
  set total-exports-milk-cream []
  set total-exports-yoghurt []
  set total-exports-butter []
  set total-exports-cheese []
end

to setup-per-capita-production-records
  set production-per-capita-beef []
  set production-per-capita-pork []
  set production-per-capita-lamb []
  set production-per-capita-chicken []
  set production-per-capita-eggs []
  set production-per-capita-wool []
  set production-per-capita-rawmilk []
end

to setup-cultured-meat
  file-close-all
  file-open "Data/NO_CulturedProtein/ProcessedData/NO_CulturedMeat.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-scenario item 0 row
    let current-meat item 1 row
    let current-cost item 2 row
    if current-scenario = cm-scenario [
      if current-meat = "Beef" [ set cm-cost-beef current-cost ]
      if current-meat = "Pork" [ set cm-cost-pork current-cost ]
      if current-meat = "Lamb" [ set cm-cost-lamb current-cost ]
      if current-meat = "Chicken" [ set cm-cost-chicken current-cost ]
    ]
  ]
  file-close
end

to setup-biosynthetic-liquid
  file-close-all
  file-open "Data/NO_CulturedProtein/ProcessedData/NO_BiosyntheticDairyAndEggs.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-scenario item 0 row
    let current-product item 1 row
    let current-cost item 2 row
    if current-scenario = pf-scenario [
      if current-product = "Dairy" [ set pf-cost-dairy current-cost ]
      if current-product = "Eggs" [ set pf-cost-eggs current-cost ]
    ]
  ]
  file-close
end

to setup-emissions
  file-close-all
  file-open "Data/NO_Emissions/ProcessedData/NO_NetLogoEmissions.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-activity item 0 row
    let current-emissions item 1 row
    if current-activity = "Barley" [ set emissions-ha-barley current-emissions ]
    if current-activity = "Oats" [ set emissions-ha-oats current-emissions ]
    if current-activity = "Wheat" [ set emissions-ha-wheat current-emissions ]
    if current-activity = "Rye & triticale" [ set emissions-ha-rye-triticale current-emissions ]
    if current-activity = "Oilseeds" [ set emissions-ha-oilseeds current-emissions ]
    if current-activity = "Potatoes" [ set emissions-ha-potatoes current-emissions ]
    if current-activity = "Vegetables" [ set emissions-ha-vegetables current-emissions ]
    if current-activity = "Green fodder & silage" [ set emissions-ha-fodder-silage current-emissions ]
    if current-activity = "Other crops" [ set emissions-ha-other-crops current-emissions ]
    if current-activity = "Orchards" [ set emissions-ha-orchards current-emissions ]
    if current-activity = "Berries" [ set emissions-ha-berries current-emissions ]
    if current-activity = "Dairy cows" [ set emissions-head-dairy-cows current-emissions ]
    if current-activity = "Beef cows" [ set emissions-head-beef-cows current-emissions ]
    if current-activity = "Other cattle" [ set emissions-head-other-cattle current-emissions ]
    if current-activity = "Sheep" [ set emissions-head-sheep current-emissions ]
    if current-activity = "Pigs" [ set emissions-head-pigs current-emissions ]
    if current-activity = "Broilers" [ set emissions-head-broilers current-emissions ]
    if current-activity = "Laying hens" [ set emissions-head-laying-hens current-emissions ]
  ]
  file-close
end

to set-params-to-default
  set sim-end-yr 2050
  if start-yr = 2013 [ set num-farms-to-sim 42437 ]
  if start-yr = 2020 [ set num-farms-to-sim 40382 ]
  set animal-yield-trajectory "Constant"
  set population-growth "Medium"
  set farm-income-viability -15
  set slaughter-min-capacity 80
  set max-dist-to-slaughter 230
  set dairy-min-capacity 80
  set max-dist-to-dairy 140
  set price-scenario "Determined by markets"
  set price-baseline-year 2020
  set price-response-ratio 1
  set sim-cm? TRUE
  ; Note that with the cm-init-yr and the pf-init-yr, it will be the next year before production comes
  ; online. This reflects the lag between commisioning and openning such facilities.
  set cm-init-yr 2024
  set cm-factory-capacity 5000
  set cm-scenario "Scenario 8"
  set cm-max-share 53.9
  set sim-pf? TRUE
  set pf-init-yr 2024
  set pf-factory-dairy-capacity 170000
  set pf-factory-egg-capacity 5000
  set pf-scenario "Scenario 2"
  set pf-max-share 53.9
  set efficiency-gain-multiplier 0.95
  set efficiency-step-int-nonmilk 20000
  set efficiency-step-int-milk 340000
  set price-growth-beef 0
  set price-growth-pork 0
  set price-growth-lamb 0
  set price-growth-chicken 0
  set price-growth-eggs 0
  set price-growth-raw-milk 0
  set price-growth-wool 0
  set price-growth-crops 0
  set consum-growth-beef 0
  set consum-growth-pork 0
  set consum-growth-lamb 0
  set consum-growth-chicken 0
  set consum-growth-eggs 0
  set consum-growth-milk-cream 0
  set consum-growth-yoghurt 0
  set consum-growth-butter 0
  set consum-growth-cheese 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SIMULATE                                                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ;;;;;;;;;;;;;;;;;;;;;;;
  ; Run farm procedures ;
  ;;;;;;;;;;;;;;;;;;;;;;;

  ask farms with [ active? = TRUE ] [
    ; Farms calculate their livestock yields and send it to dairies and slaughterhouses.
    let my-production-raw-milk farm-production-raw-milk
    let my-production-beef farm-production-beef
    let my-production-pork farm-production-pork
    let my-production-lamb farm-production-lamb
    let my-production-chicken farm-production-chicken
    let my-production-eggs farm-production-eggs
    ask my-farm-dairy-links [
      ask other-end [
        set supply-rawmilk (supply-rawmilk + my-production-raw-milk)
      ]
    ]
    ask my-farm-slaughterhouse-links [
      ask other-end [
        set supply-beef (supply-beef + my-production-beef)
        set supply-pork (supply-pork + my-production-pork)
        set supply-lamb (supply-lamb + my-production-lamb)
        set supply-chicken (supply-chicken + my-production-chicken)
        set supply-eggs (supply-eggs + my-production-eggs)
      ]
    ]
    ; Farms calculate their emissions for the year.
    set farm-emissions (ha-barley * emissions-ha-barley) + (ha-oats * emissions-ha-oats) + (ha-wheat * emissions-ha-wheat) + (ha-rye-triticale * emissions-ha-rye-triticale) + (ha-oilseeds * emissions-ha-oilseeds) + (ha-potatoes * emissions-ha-potatoes) + (ha-vegetables * emissions-ha-vegetables) + (ha-fodder-silage * emissions-ha-fodder-silage) + (ha-other-crops * emissions-ha-other-crops) + (ha-orchards * emissions-ha-orchards) + (ha-berries * emissions-ha-berries) + (num-dairy-cows * emissions-head-dairy-cows) + (num-beef-cows * emissions-head-beef-cows) + (num-other-cattle * emissions-head-other-cattle) + (num-sheep * emissions-head-sheep) + (num-pigs * emissions-head-pigs) + (num-broilers * emissions-head-broilers) + (num-laying-hens * emissions-head-laying-hens)
    ; Farms calculate their annual income from crops, livestock, poultry, subsidies, and grants.
    let current-farm-income farm-income
    ; Farms note their income each year.
    set annual-income lput list ( year ) ( current-farm-income ) annual-income
    ; Add the income to the recent-income list as well. This stores just the income values from the last
    ; five years as we drop the first value in the list each year post-2017.
    if Year <= start-yr + 4 [ set recent-income lput current-farm-income recent-income ]
    if Year > start-yr + 4 [
      set recent-income remove-item 0 recent-income
      set recent-income lput current-farm-income recent-income
    ]
    ; The benchmark income is the mean income of each farm as determined during the first five
    ; years of the simulations. Prior to five year point, the benchmark is set to the mean
    ; annual income since the start of the simulation.
    if Year = start-yr [ set benchmark-income current-farm-income ]
    if Year = start-yr + 1 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income)) / 2 ]
    if Year = start-yr + 2 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income) + (item 1 item 2 annual-income)) / 3 ]
    if Year = start-yr + 3 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income) + (item 1 item 2 annual-income) + (item 1 item 3 annual-income)) / 4 ]
    if Year = start-yr + 4 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income) + (item 1 item 2 annual-income) + (item 1 item 3 annual-income) + (item 1 item 4 annual-income)) / 5 ]
    ; For all years after the initialisation year, farms compare their mean recent-income against
    ; this benchmark income. If it is more than the % specified by the farm-income-viability
    ; parameter below their benchmark income, they will cease operations.
    if Year > start-yr and mean recent-income < (benchmark-income - abs(farm-income-viability) * (benchmark-income / 100)) [
      set active? FALSE
      set partner-slaughterhouse 0
      set partner-dairy 0
      set farm-emissions 0
      ask my-links [die]
      set color [77 20 106 125]
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; Run dairy procedures ;
  ;;;;;;;;;;;;;;;;;;;;;;;;

  ask dairies [
    ; Dairies note the volume of raw milk they have for processing in their historical records.
    set processed-rawmilk lput list ( year ) ( supply-rawmilk ) processed-rawmilk
    ; Around 11% of milk supply is wasted during processing. We deduct this first.
    set supply-rawmilk supply-rawmilk - ((supply-rawmilk / 100) * 0.1105857)
    ; The remaining raw milk is transformed into dairy goods. Production of dairy goods is
    ; in proportion to current demand for those goods (see `to-report raw-milk-division-ratio`
    ; for further details of how this is calculated). After determining the share of raw milk
    ; dedicated to the production of each good, a conversion factor is applied which shows how
    ; 1kg of raw milk translates into 1kg of the good.
    set wholesale-stock-milk-cream (supply-rawmilk * item 0 raw-milk-division-ratio) / 1
    set wholesale-stock-yoghurt (supply-rawmilk * item 1 raw-milk-division-ratio) / 1.13
    set wholesale-stock-butter (supply-rawmilk * item 2 raw-milk-division-ratio) / 0.252
    set wholesale-stock-cheese (supply-rawmilk * item 3 raw-milk-division-ratio) / 6.91
    ; With all raw milk converted to dairy products, the raw milk supply of dairies is reset to 0.
    set supply-rawmilk 0
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Run slaughterhouse procedures ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask slaughterhouses with [ active? = TRUE ] [
    ; Slaughterhouses note the quantity of carcass they have for processing in their historical records.
    set processed-beef lput list ( year ) ( supply-beef ) processed-beef
    set processed-pork lput list ( year ) ( supply-pork ) processed-pork
    set processed-lamb lput list ( year ) ( supply-lamb ) processed-lamb
    set processed-chicken lput list ( year ) ( supply-chicken ) processed-chicken
    set processed-eggs lput list ( year ) ( supply-eggs ) processed-eggs
    ; Slaughterhouses convert their carcass supplies into meat goods. In this version of the
    ; model, carcass is simply converted into meat products on a one-to-one basis.
    set wholesale-stock-beef supply-beef
    set wholesale-stock-pork supply-pork
    set wholesale-stock-lamb supply-lamb
    set wholesale-stock-chicken supply-chicken
    set wholesale-stock-eggs supply-eggs
    ; With all carcass converted to meat products, the carcass supply of slaughterhouses is reset to 0.
    set supply-beef 0
    set supply-pork 0
    set supply-lamb 0
    set supply-chicken 0
    set supply-eggs 0
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Account for production of cultured meat facilities ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask cm-factories [
    set wholesale-stock cm-factory-capacity
    set processed-meat lput list ( year ) ( cm-factory-capacity ) processed-meat
    set factory-emissions cm-factory-capacity * (emissions-cm-meat * 1000)
  ]

  ; Update global storing details of aggregate CM meat production.
  let latest-production-cm-meat 0
  ask cm-factories [
    let my-latest-production-cm-meat filter [ i -> Year = item 0 i ] processed-meat
    set my-latest-production-cm-meat item 1 item 0 my-latest-production-cm-meat
    set latest-production-cm-meat latest-production-cm-meat + my-latest-production-cm-meat
  ]
  set aggregate-production-cm-meat aggregate-production-cm-meat + latest-production-cm-meat

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Account for production of biosynthetic liquid facilities ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask pf-factories [
    if product-type = "Dairy" [
      ; Biosynthetic dairy factories produce a quantity of raw milk equal to pf-factory-dairy-capacity. They
      ; transform this into dairy goods in the same way that dairies do. Production of dairy goods is in
      ; proportion to current demand for those goods (see `to-report raw-milk-division-ratio` for further
      ; details of how this is calculated). After determining the share of raw milk dedicated to the
      ; production of each good, a conversion factor is applied which shows how 1kg of raw milk translates
      ; into 1kg of the good.
      set wholesale-stock-milk-cream (pf-factory-dairy-capacity * item 0 raw-milk-division-ratio) / 1
      set wholesale-stock-yoghurt (pf-factory-dairy-capacity * item 1 raw-milk-division-ratio) / 1.13
      set wholesale-stock-butter (pf-factory-dairy-capacity * item 2 raw-milk-division-ratio) / 0.252
      set wholesale-stock-cheese (pf-factory-dairy-capacity * item 3 raw-milk-division-ratio) / 6.91
      set wholesale-stock-eggs 0
      set processed-rawmilk lput list ( year ) ( pf-factory-dairy-capacity ) processed-rawmilk
      set factory-emissions pf-factory-dairy-capacity * (emissions-pf-dairy * 1000)
    ]
    if product-type = "Eggs" [
      ; In contrast, eggs production is simply determined by the egg production capacity slider.
      set wholesale-stock-eggs pf-factory-egg-capacity
      set wholesale-stock-milk-cream 0
      set wholesale-stock-yoghurt 0
      set wholesale-stock-butter 0
      set wholesale-stock-cheese 0
      set processed-eggs lput list ( year ) ( pf-factory-egg-capacity ) processed-eggs
      set factory-emissions pf-factory-egg-capacity * (emissions-pf-egg * 1000)
    ]
  ]

  ; Update globals storing details of aggregate PF raw milk and egg production.
  let latest-production-pf-dairy 0
  ask pf-factories with [ product-type = "Dairy" ] [
    let my-latest-production-pf-dairy filter [ i -> Year = item 0 i ] processed-rawmilk
    set my-latest-production-pf-dairy item 1 item 0 my-latest-production-pf-dairy
    set latest-production-pf-dairy latest-production-pf-dairy + my-latest-production-pf-dairy
  ]
  set aggregate-production-pf-dairy aggregate-production-pf-dairy + latest-production-pf-dairy

  let latest-production-pf-eggs 0
  ask pf-factories with [ product-type = "Eggs" ] [
    let my-latest-production-pf-eggs filter [ i -> Year = item 0 i ] processed-eggs
    set my-latest-production-pf-eggs item 1 item 0 my-latest-production-pf-eggs
    set latest-production-pf-eggs latest-production-pf-eggs + my-latest-production-pf-eggs
  ]
  set aggregate-production-pf-eggs aggregate-production-pf-eggs + latest-production-pf-eggs

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; Simulate consumption ;
  ;;;;;;;;;;;;;;;;;;;;;;;;

  ; Simulate consumption by transfering stocks from slaughterhouses/dairies to kommuner or exporting through checkpoints.
  ; 1. Beef
  ask kommuner [
    let required-beef local-consumption-beef
    ; While kommuner consumption requirements are yet to be fully met and while there are slaughterhouses with stock, identify the nearest slaughterhouse with available stock and buy.
    while [ required-beef > 0 and count slaughterhouses with [ wholesale-stock-beef > 0 ] > 0 ] [
      let slaughterhouse-to-buy-from min-one-of (slaughterhouses with [ wholesale-stock-beef > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-beef ] of slaughterhouse-to-buy-from > required-beef [
        ask slaughterhouse-to-buy-from [ set wholesale-stock-beef wholesale-stock-beef - required-beef ]
        set required-beef 0
      ] [
        set required-beef required-beef - [ wholesale-stock-beef ] of slaughterhouse-to-buy-from
        ask slaughterhouse-to-buy-from [ set wholesale-stock-beef 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse supplies are exhausted, seek stocks from cultured meat factories.
    if required-beef > 0 and count cm-factories with [ meat-type = "Beef" and wholesale-stock > 0 ] > 0 [
      while [ required-beef > 0 and count cm-factories with [ meat-type = "Beef" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ meat-type = "Beef" and wholesale-stock > 0 ]
        ifelse [ wholesale-stock ] of cm-factory-to-buy-from > required-beef [
          ask cm-factory-to-buy-from [ set wholesale-stock wholesale-stock - required-beef ]
          set required-beef 0
        ] [
          set required-beef required-beef - [ wholesale-stock ] of cm-factory-to-buy-from
          ask cm-factory-to-buy-from[ set wholesale-stock 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse and cultured meat factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-beef > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-beef imports-beef + required-beef ]
      set required-beef 0
    ]
  ]
  ; If slaughterhouses and cultured meat factories have stock left after kommuner have purchased what they need, the stock is exported.
  ask slaughterhouses with [ wholesale-stock-beef > 0 ] [
    let remaining-stock wholesale-stock-beef
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-beef exports-beef + remaining-stock ]
    set wholesale-stock-beef 0
  ]
  ask cm-factories with [ meat-type = "Beef" and wholesale-stock > 0 ] [
    let remaining-stock wholesale-stock
    ask one-of checkpoints [ set exports-beef exports-beef + remaining-stock ]
    set wholesale-stock 0
  ]
  ; 2. Pork
  ask kommuner [
    let required-pork local-consumption-pork
    ; While kommuner consumption requirements are yet to be fully met and while there are slaughterhouses with stock, identify the nearest slaughterhouse with available stock and buy.
    while [ required-pork > 0 and count slaughterhouses with [ wholesale-stock-pork > 0 ] > 0 ] [
      let slaughterhouse-to-buy-from min-one-of (slaughterhouses with [ wholesale-stock-pork > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-pork ] of slaughterhouse-to-buy-from > required-pork [
        ask slaughterhouse-to-buy-from [ set wholesale-stock-pork wholesale-stock-pork - required-pork ]
        set required-pork 0
      ] [
        set required-pork required-pork - [ wholesale-stock-pork ] of slaughterhouse-to-buy-from
        ask slaughterhouse-to-buy-from [ set wholesale-stock-pork 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse supplies are exhausted, seek stocks from cultured meat factories.
    if required-pork > 0 and count cm-factories with [ meat-type = "Pork" and wholesale-stock > 0 ] > 0 [
      while [ required-pork > 0 and count cm-factories with [ meat-type = "Pork" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ meat-type = "Pork" and wholesale-stock > 0 ]
        ifelse [ wholesale-stock ] of cm-factory-to-buy-from > required-pork [
          ask cm-factory-to-buy-from [ set wholesale-stock wholesale-stock - required-pork ]
          set required-pork 0
        ] [
          set required-pork required-pork - [ wholesale-stock ] of cm-factory-to-buy-from
          ask cm-factory-to-buy-from[ set wholesale-stock 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse and cultured meat factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-pork > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-pork imports-pork + required-pork ]
      set required-pork 0
    ]
  ]
  ; If slaughterhouses and cultured meat factories have stock left after kommuner have purchased what they need, the stock is exported.
  ask slaughterhouses with [ wholesale-stock-pork > 0 ] [
    let remaining-stock wholesale-stock-pork
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-pork exports-pork + remaining-stock ]
    set wholesale-stock-pork 0
  ]
  ask cm-factories with [ meat-type = "Pork" and wholesale-stock > 0 ] [
    let remaining-stock wholesale-stock
    ask one-of checkpoints [ set exports-pork exports-pork + remaining-stock ]
    set wholesale-stock 0
  ]
  ; 3. Lamb
  ask kommuner [
    let required-lamb local-consumption-lamb
    ; While kommuner consumption requirements are yet to be fully met and while there are slaughterhouses with stock, identify the nearest slaughterhouse with available stock and buy.
    while [ required-lamb > 0 and count slaughterhouses with [ wholesale-stock-lamb > 0 ] > 0 ] [
      let slaughterhouse-to-buy-from min-one-of (slaughterhouses with [ wholesale-stock-lamb > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-lamb ] of slaughterhouse-to-buy-from > required-lamb [
        ask slaughterhouse-to-buy-from [ set wholesale-stock-lamb wholesale-stock-lamb - required-lamb ]
        set required-lamb 0
      ] [
        set required-lamb required-lamb - [ wholesale-stock-lamb ] of slaughterhouse-to-buy-from
        ask slaughterhouse-to-buy-from [ set wholesale-stock-lamb 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse supplies are exhausted, seek stocks from cultured meat factories.
    if required-lamb > 0 and count cm-factories with [ meat-type = "Lamb" and wholesale-stock > 0 ] > 0 [
      while [ required-lamb > 0 and count cm-factories with [ meat-type = "Lamb" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ meat-type = "Lamb" and wholesale-stock > 0 ]
        ifelse [ wholesale-stock ] of cm-factory-to-buy-from > required-lamb [
          ask cm-factory-to-buy-from [ set wholesale-stock wholesale-stock - required-lamb ]
          set required-lamb 0
        ] [
          set required-lamb required-lamb - [ wholesale-stock ] of cm-factory-to-buy-from
          ask cm-factory-to-buy-from[ set wholesale-stock 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse and cultured meat factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-lamb > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-lamb imports-lamb + required-lamb ]
      set required-lamb 0
    ]
  ]
  ; If slaughterhouses and cultured meat factories have stock left after kommuner have purchased what they need, the stock is exported.
  ask slaughterhouses with [ wholesale-stock-lamb > 0 ] [
    let remaining-stock wholesale-stock-lamb
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-lamb exports-lamb + remaining-stock ]
    set wholesale-stock-lamb 0
  ]
  ask cm-factories with [ meat-type = "Lamb" and wholesale-stock > 0 ] [
    let remaining-stock wholesale-stock
    ask one-of checkpoints [ set exports-lamb exports-lamb + remaining-stock ]
    set wholesale-stock 0
  ]
  ; 4. Chicken
  ask kommuner [
    let required-chicken local-consumption-chicken
    ; While kommuner consumption requirements are yet to be fully met and while there are slaughterhouses with stock, identify the nearest slaughterhouse with available stock and buy.
    while [ required-chicken > 0 and count slaughterhouses with [ wholesale-stock-chicken > 0 ] > 0 ] [
      let slaughterhouse-to-buy-from min-one-of (slaughterhouses with [ wholesale-stock-chicken > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-chicken ] of slaughterhouse-to-buy-from > required-chicken [
        ask slaughterhouse-to-buy-from [ set wholesale-stock-chicken wholesale-stock-chicken - required-chicken ]
        set required-chicken 0
      ] [
        set required-chicken required-chicken - [ wholesale-stock-chicken ] of slaughterhouse-to-buy-from
        ask slaughterhouse-to-buy-from [ set wholesale-stock-chicken 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse supplies are exhausted, seek stocks from cultured meat factories.
    if required-chicken > 0 and count cm-factories with [ meat-type = "Chicken" and wholesale-stock > 0 ] > 0 [
      while [ required-chicken > 0 and count cm-factories with [ meat-type = "Chicken" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ meat-type = "Chicken" and wholesale-stock > 0 ]
        ifelse [ wholesale-stock ] of cm-factory-to-buy-from > required-chicken [
          ask cm-factory-to-buy-from [ set wholesale-stock wholesale-stock - required-chicken ]
          set required-chicken 0
        ] [
          set required-chicken required-chicken - [ wholesale-stock ] of cm-factory-to-buy-from
          ask cm-factory-to-buy-from[ set wholesale-stock 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse and cultured meat factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-chicken > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-chicken imports-chicken + required-chicken ]
      set required-chicken 0
    ]
  ]
  ; If slaughterhouses and cultured meat factories have stock left after kommuner have purchased what they need, the stock is exported.
  ask slaughterhouses with [ wholesale-stock-chicken > 0 ] [
    let remaining-stock wholesale-stock-chicken
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-chicken exports-chicken + remaining-stock ]
    set wholesale-stock-chicken 0
  ]
  ask cm-factories with [ meat-type = "Chicken" and wholesale-stock > 0 ] [
    let remaining-stock wholesale-stock
    ask one-of checkpoints [ set exports-chicken exports-chicken + remaining-stock ]
    set wholesale-stock 0
  ]
  ; 5. Eggs
  ask kommuner [
    let required-eggs local-consumption-eggs
    ; While kommuner consumption requirements are yet to be fully met and while there are slaughterhouses with stock, identify the nearest slaughterhouse with available stock and buy.
    while [ required-eggs > 0 and count slaughterhouses with [ wholesale-stock-eggs > 0 ] > 0 ] [
      let slaughterhouse-to-buy-from min-one-of (slaughterhouses with [ wholesale-stock-eggs > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-eggs ] of slaughterhouse-to-buy-from > required-eggs [
        ask slaughterhouse-to-buy-from [ set wholesale-stock-eggs wholesale-stock-eggs - required-eggs ]
        set required-eggs 0
      ] [
        set required-eggs required-eggs - [ wholesale-stock-eggs ] of slaughterhouse-to-buy-from
        ask slaughterhouse-to-buy-from [ set wholesale-stock-eggs 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse supplies are exhausted, seek stocks from biosynthetic liquid factories.
    if required-eggs > 0 and count pf-factories with [ product-type = "Eggs" and wholesale-stock-eggs > 0 ] > 0 [
      while [ required-eggs > 0 and count pf-factories with [ product-type = "Eggs" and wholesale-stock-eggs > 0 ] > 0 ] [
        let pf-factory-to-buy-from one-of pf-factories with [ product-type = "Eggs" and wholesale-stock-eggs > 0 ]
        ifelse [ wholesale-stock-eggs ] of pf-factory-to-buy-from > required-eggs [
          ask pf-factory-to-buy-from [ set wholesale-stock-eggs wholesale-stock-eggs - required-eggs ]
          set required-eggs 0
        ] [
          set required-eggs required-eggs - [ wholesale-stock-eggs ] of pf-factory-to-buy-from
          ask pf-factory-to-buy-from [ set wholesale-stock-eggs 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but slaughterhouse and factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-eggs > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-eggs imports-eggs + required-eggs ]
      set required-eggs 0
    ]
  ]
  ; If slaughterhouses and biosynthetic liquid factories have stock left after kommuner have purchased what they need, the stock is exported.
  ask slaughterhouses with [ wholesale-stock-eggs > 0 ] [
    let remaining-stock wholesale-stock-eggs
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-eggs exports-eggs + remaining-stock ]
    set wholesale-stock-eggs 0
  ]
  ask pf-factories with [ product-type = "Eggs" and wholesale-stock-eggs > 0 ] [
    let remaining-stock wholesale-stock-eggs
    ask one-of checkpoints [ set exports-eggs exports-eggs + remaining-stock ]
    set wholesale-stock-eggs 0
  ]
  ; 6. Milk and cream
  ask kommuner [
    let required-milk-cream local-consumption-milk-cream
    ; While kommuner consumption requirements are yet to be fully met and while there are dairies with stock, identify the nearest dairy with available stock and buy.
    while [ required-milk-cream > 0 and count dairies with [ wholesale-stock-milk-cream > 0 ] > 0 ] [
      let dairy-to-buy-from min-one-of (dairies with [ wholesale-stock-milk-cream > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-milk-cream ] of dairy-to-buy-from > required-milk-cream [
        ask dairy-to-buy-from [ set wholesale-stock-milk-cream wholesale-stock-milk-cream - required-milk-cream ]
        set required-milk-cream 0
      ] [
        set required-milk-cream required-milk-cream - [ wholesale-stock-milk-cream ] of dairy-to-buy-from
        ask dairy-to-buy-from [ set wholesale-stock-milk-cream 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy supplies are exhausted, seek stocks from biosynthetic liquid factories.
    if required-milk-cream > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-milk-cream > 0 ] > 0 [
      while [ required-milk-cream > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-milk-cream > 0 ] > 0 ] [
        let pf-factory-to-buy-from one-of pf-factories with [ product-type = "Dairy" and wholesale-stock-milk-cream > 0 ]
        ifelse [ wholesale-stock-milk-cream ] of pf-factory-to-buy-from > required-milk-cream [
          ask pf-factory-to-buy-from [ set wholesale-stock-milk-cream wholesale-stock-milk-cream - required-milk-cream ]
          set required-milk-cream 0
        ] [
          set required-milk-cream required-milk-cream - [ wholesale-stock-milk-cream ] of pf-factory-to-buy-from
          ask pf-factory-to-buy-from [ set wholesale-stock-milk-cream 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy and factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-milk-cream > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-milk-cream imports-milk-cream + required-milk-cream ]
      set required-milk-cream 0
    ]
  ]
  ; If dairies have stock left after kommuner have purchased what they need, the stock is exported.
  ask dairies with [ wholesale-stock-milk-cream > 0 ] [
    let remaining-stock wholesale-stock-milk-cream
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-milk-cream exports-milk-cream + remaining-stock ]
    set wholesale-stock-milk-cream 0
  ]
  ask pf-factories with [ product-type = "Dairy" and wholesale-stock-milk-cream > 0 ] [
    let remaining-stock wholesale-stock-milk-cream
    ask one-of checkpoints [ set exports-milk-cream exports-milk-cream + remaining-stock ]
    set wholesale-stock-milk-cream 0
  ]
  ; 7. Yoghurt
  ask kommuner [
    let required-yoghurt local-consumption-yoghurt
    ; While kommuner consumption requirements are yet to be fully met and while there are dairies with stock, identify the nearest dairy with available stock and buy.
    while [ required-yoghurt > 0 and count dairies with [ wholesale-stock-yoghurt > 0 ] > 0 ] [
      let dairy-to-buy-from min-one-of (dairies with [ wholesale-stock-yoghurt > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-yoghurt ] of dairy-to-buy-from > required-yoghurt [
        ask dairy-to-buy-from [ set wholesale-stock-yoghurt wholesale-stock-yoghurt - required-yoghurt ]
        set required-yoghurt 0
      ] [
        set required-yoghurt required-yoghurt - [ wholesale-stock-yoghurt ] of dairy-to-buy-from
        ask dairy-to-buy-from [ set wholesale-stock-yoghurt 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy supplies are exhausted, seek stocks from biosynthetic liquid factories.
    if required-yoghurt > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-yoghurt > 0 ] > 0 [
      while [ required-yoghurt > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-yoghurt > 0 ] > 0 ] [
        let pf-factory-to-buy-from one-of pf-factories with [ product-type = "Dairy" and wholesale-stock-yoghurt > 0 ]
        ifelse [ wholesale-stock-yoghurt ] of pf-factory-to-buy-from > required-yoghurt [
          ask pf-factory-to-buy-from [ set wholesale-stock-yoghurt wholesale-stock-yoghurt - required-yoghurt ]
          set required-yoghurt 0
        ] [
          set required-yoghurt required-yoghurt - [ wholesale-stock-yoghurt ] of pf-factory-to-buy-from
          ask pf-factory-to-buy-from [ set wholesale-stock-yoghurt 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy and factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-yoghurt > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-yoghurt imports-yoghurt + required-yoghurt ]
      set required-yoghurt 0
    ]
  ]
  ; If dairies have stock left after kommuner have purchased what they need, the stock is exported.
  ask dairies with [ wholesale-stock-yoghurt > 0 ] [
    let remaining-stock wholesale-stock-yoghurt
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-yoghurt exports-yoghurt + remaining-stock ]
    set wholesale-stock-yoghurt 0
  ]
  ask pf-factories with [ product-type = "Dairy" and wholesale-stock-yoghurt > 0 ] [
    let remaining-stock wholesale-stock-yoghurt
    ask one-of checkpoints [ set exports-yoghurt exports-yoghurt + remaining-stock ]
    set wholesale-stock-yoghurt 0
  ]
  ; 8. Butter
  ask kommuner [
    let required-butter local-consumption-butter
    ; While kommuner consumption requirements are yet to be fully met and while there are dairies with stock, identify the nearest dairy with available stock and buy.
    while [ required-butter > 0 and count dairies with [ wholesale-stock-butter > 0 ] > 0 ] [
      let dairy-to-buy-from min-one-of (dairies with [ wholesale-stock-butter > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-butter ] of dairy-to-buy-from > required-butter [
        ask dairy-to-buy-from [ set wholesale-stock-butter wholesale-stock-butter - required-butter ]
        set required-butter 0
      ] [
        set required-butter required-butter - [ wholesale-stock-butter ] of dairy-to-buy-from
        ask dairy-to-buy-from [ set wholesale-stock-butter 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy supplies are exhausted, seek stocks from biosynthetic liquid factories.
    if required-butter > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-butter > 0 ] > 0 [
      while [ required-butter > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-butter > 0 ] > 0 ] [
        let pf-factory-to-buy-from one-of pf-factories with [ product-type = "Dairy" and wholesale-stock-butter > 0 ]
        ifelse [ wholesale-stock-butter ] of pf-factory-to-buy-from > required-butter [
          ask pf-factory-to-buy-from [ set wholesale-stock-butter wholesale-stock-butter - required-butter ]
          set required-butter 0
        ] [
          set required-butter required-butter - [ wholesale-stock-butter ] of pf-factory-to-buy-from
          ask pf-factory-to-buy-from [ set wholesale-stock-butter 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy and factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-butter > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-butter imports-butter + required-butter ]
      set required-butter 0
    ]
  ]
  ; If dairies have stock left after kommuner have purchased what they need, the stock is exported.
  ask dairies with [ wholesale-stock-butter > 0 ] [
    let remaining-stock wholesale-stock-butter
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-butter exports-butter + remaining-stock ]
    set wholesale-stock-butter 0
  ]
  ask pf-factories with [ product-type = "Dairy" and wholesale-stock-butter > 0 ] [
    let remaining-stock wholesale-stock-butter
    ask one-of checkpoints [ set exports-butter exports-butter + remaining-stock ]
    set wholesale-stock-butter 0
  ]
  ; 9. Cheese
  ask kommuner [
    let required-cheese local-consumption-cheese
    ; While kommuner consumption requirements are yet to be fully met and while there are dairies with stock, identify the nearest dairy with available stock and buy.
    while [ required-cheese > 0 and count dairies with [ wholesale-stock-cheese > 0 ] > 0 ] [
      let dairy-to-buy-from min-one-of (dairies with [ wholesale-stock-cheese > 0 ]) [ distance myself ]
      ifelse [ wholesale-stock-cheese ] of dairy-to-buy-from > required-cheese [
        ask dairy-to-buy-from [ set wholesale-stock-cheese wholesale-stock-cheese - required-cheese ]
        set required-cheese 0
      ] [
        set required-cheese required-cheese - [ wholesale-stock-cheese ] of dairy-to-buy-from
        ask dairy-to-buy-from [ set wholesale-stock-cheese 0 ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy supplies are exhausted, seek stocks from biosynthetic liquid factories.
    if required-cheese > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-cheese > 0 ] > 0 [
      while [ required-cheese > 0 and count pf-factories with [ product-type = "Dairy" and wholesale-stock-cheese > 0 ] > 0 ] [
        let pf-factory-to-buy-from one-of pf-factories with [ product-type = "Dairy" and wholesale-stock-cheese > 0 ]
        ifelse [ wholesale-stock-cheese ] of pf-factory-to-buy-from > required-cheese [
          ask pf-factory-to-buy-from [ set wholesale-stock-cheese wholesale-stock-cheese - required-cheese ]
          set required-cheese 0
        ] [
          set required-cheese required-cheese - [ wholesale-stock-cheese ] of pf-factory-to-buy-from
          ask pf-factory-to-buy-from [ set wholesale-stock-cheese 0 ]
        ]
      ]
    ]
    ; If consumption requirements are yet to be fully met but dairy and factory supplies are exhausted, import the remainder of what is needed from the nearest checkpoint.
    if required-cheese > 0 [
      let nearest-checkpoint min-one-of checkpoints [ distance myself ]
      ask nearest-checkpoint [ set imports-cheese imports-cheese + required-cheese ]
      set required-cheese 0
    ]
  ]
  ; If dairies have stock left after kommuner have purchased what they need, the stock is exported.
  ask dairies with [ wholesale-stock-cheese > 0 ] [
    let remaining-stock wholesale-stock-cheese
    let nearest-checkpoint min-one-of checkpoints [ distance myself ]
    ask nearest-checkpoint [ set exports-cheese exports-cheese + remaining-stock ]
    set wholesale-stock-cheese 0
  ]
  ask pf-factories with [ product-type = "Dairy" and wholesale-stock-cheese > 0 ] [
    let remaining-stock wholesale-stock-cheese
    ask one-of checkpoints [ set exports-cheese exports-cheese + remaining-stock ]
    set wholesale-stock-cheese 0
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Calculate total nationwide imports/exports ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  set total-imports-beef lput list ( year ) ( sum [ imports-beef ] of checkpoints ) total-imports-beef
  set total-imports-pork lput list ( year ) ( sum [ imports-pork ] of checkpoints ) total-imports-pork
  set total-imports-lamb lput list ( year ) ( sum [ imports-lamb ] of checkpoints ) total-imports-lamb
  set total-imports-chicken lput list ( year ) ( sum [ imports-chicken ] of checkpoints ) total-imports-chicken
  set total-imports-eggs lput list ( year ) ( sum [ imports-eggs ] of checkpoints ) total-imports-eggs
  set total-imports-milk-cream lput list ( year ) ( sum [ imports-milk-cream ] of checkpoints ) total-imports-milk-cream
  set total-imports-yoghurt lput list ( year ) ( sum [ imports-yoghurt ] of checkpoints ) total-imports-yoghurt
  set total-imports-butter lput list ( year ) ( sum [ imports-butter ] of checkpoints ) total-imports-butter
  set total-imports-cheese lput list ( year ) ( sum [ imports-cheese ] of checkpoints ) total-imports-cheese
  set total-exports-beef lput list ( year ) ( sum [ exports-beef ] of checkpoints ) total-exports-beef
  set total-exports-pork lput list ( year ) ( sum [ exports-pork ] of checkpoints ) total-exports-pork
  set total-exports-lamb lput list ( year ) ( sum [ exports-lamb ] of checkpoints ) total-exports-lamb
  set total-exports-chicken lput list ( year ) ( sum [ exports-chicken ] of checkpoints ) total-exports-chicken
  set total-exports-eggs lput list ( year ) ( sum [ exports-eggs ] of checkpoints ) total-exports-eggs
  set total-exports-milk-cream lput list ( year ) ( sum [ exports-milk-cream ] of checkpoints ) total-exports-milk-cream
  set total-exports-yoghurt lput list ( year ) ( sum [ exports-yoghurt ] of checkpoints ) total-exports-yoghurt
  set total-exports-butter lput list ( year ) ( sum [ exports-butter ] of checkpoints ) total-exports-butter
  set total-exports-cheese lput list ( year ) ( sum [ exports-cheese ] of checkpoints ) total-exports-cheese

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Reset the checkpoint import/export values before proceeding to the next year ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask checkpoints [
    set imports-beef 0
    set imports-pork 0
    set imports-lamb 0
    set imports-chicken 0
    set imports-eggs 0
    set imports-milk-cream 0
    set imports-yoghurt 0
    set imports-butter 0
    set imports-cheese 0
    set exports-beef 0
    set exports-pork 0
    set exports-lamb 0
    set exports-chicken 0
    set exports-eggs 0
    set exports-milk-cream 0
    set exports-yoghurt 0
    set exports-butter 0
    set exports-cheese 0
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Assess slaughterhouse viability ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Assess the quantity of meat processed by each slaughterhouse this year. If the quantity
  ; is below the viability threshold, the slaughterhouse in question will no longer be active.
  ask slaughterhouses with [ active? = TRUE ] [
    ; Calculate the total carcass that has been processed in the current year.
    let carcass-supply-current (last last processed-beef) + (last last processed-pork) + (last last processed-lamb) + (last last processed-chicken)
    ; If the total is below the min-viable-carcass capacity, the slaughterhouse will no longer be viable.
    if carcass-supply-current < min-viable-carcass [
      set active? FALSE
      set color [138 93 146 ]
      ask link-neighbors [
        set partner-slaughterhouse 0
        set slaughterhouse-distance-km 0
      ]
      ask my-farm-slaughterhouse-links [ die ]
    ]
  ]
  ; Ask livestock and poultry farms that are no longer linked to active slaughterhouses
  ; to check whether there are any alternative active slaughterhouses within the maximum allowed
  ; distance of them. If there are not, they will cease engaging in livestock and poultry
  ; related activities.
  ask farms with [ active? = TRUE and partner-slaughterhouse = 0 and (num-dairy-cows + num-beef-cows + num-other-cattle + num-pigs + num-sheep + num-broilers + num-laying-hens ) > 0 ] [
    ifelse (count slaughterhouses with [ active? = TRUE ] in-radius (max-dist-to-slaughter / dist-coeff)) = 0 [
      set num-dairy-cows 0
      set num-beef-cows 0
      set num-other-cattle 0
      set num-sheep 0
      set num-pigs 0
      set num-broilers 0
      set num-laying-hens 0
      set subsidy-livestock 0
      set subsidy-milk 0
    ] [
      ; But if there is an active slaughterhouse within distance, they will form a link with it.
      set partner-slaughterhouse min-one-of slaughterhouses with [active? = TRUE ] [ distance myself ]
      create-farm-slaughterhouse-link-to partner-slaughterhouse [
        tie
        set color [204 116 94 125]
        if hide-farm-slaught-links? = TRUE [ set hidden? TRUE ]
      ]
      set slaughterhouse-distance-km (item 0 [link-length] of my-farm-slaughterhouse-links) * dist-coeff
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Assess dairy viability ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Assess the quantity of raw milk processed by each dairy this year. If the quantity is below
  ;the viability threshold, the dairy in question will no longer be active.
  ask dairies with [ active? = TRUE ] [
    ; Determine the total raw milk that has been processed in the current year.
    let rawmilk-supply-current (last last processed-rawmilk)
    ; If the total is below the min-viable-rawmilk capacity, the dairy will no longer be viable.
    if rawmilk-supply-current < min-viable-rawmilk [
      set active? FALSE
      set color [ 138 93 146 ]
      ask link-neighbors [
        set partner-dairy 0
        set dairy-distance-km 0
      ]
      ask my-farm-dairy-links [ die ]
    ]
  ]
  ; Ask dairy farms that are no longer linked to active dairies to check whether there are
  ; any alternative active dairies within the maximum allowed distance of them. If there
  ; are not, they will cease engaging in dairy related activities.
  ask farms with [ active? = TRUE and partner-dairy = 0 and num-dairy-cows > 0 ] [
    ifelse (count dairies with [ active? = TRUE ] in-radius (max-dist-to-dairy / dist-coeff)) = 0 [
      set num-dairy-cows 0
      set subsidy-milk 0
    ] [
      ; But if there is an active dairy within distance, they will form a link with it.
      set partner-dairy min-one-of dairies with [active? = TRUE ] [ distance myself ]
      create-farm-dairy-link-to partner-dairy [
        tie
        set color [239 213 167 125]
        if hide-farm-dairy-links? = TRUE [ set hidden? TRUE ]
      ]
      set dairy-distance-km (item 0 [link-length] of my-farm-dairy-links) * dist-coeff
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Record per capita production for current year ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  set production-per-capita-beef lput list ( year ) ( per-capita-production-beef ) production-per-capita-beef
  set production-per-capita-pork lput list ( year ) ( per-capita-production-pork ) production-per-capita-pork
  set production-per-capita-lamb lput list ( year ) ( per-capita-production-lamb ) production-per-capita-lamb
  set production-per-capita-chicken lput list ( year ) ( per-capita-production-chicken ) production-per-capita-chicken
  set production-per-capita-eggs lput list ( year ) ( per-capita-production-eggs ) production-per-capita-eggs
  set production-per-capita-wool lput list ( year ) ( per-capita-production-wool ) production-per-capita-wool
  set production-per-capita-rawmilk lput list ( year ) ( per-capita-production-rawmilk ) production-per-capita-rawmilk

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Prices are determined for the next year if they are being set by the markets ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Prices change as the per capita production changes in order to incentivise increased production where
  ; there is undersupply or to disincentivise production if there is oversupply. We use 2020 prices (known
  ; as the price-baseline-year) and per capita production as our benchmark with prices always changing
  ; proportionally to this.
  if Year >= 2020 and price-scenario = "Determined by markets" [

    ; Beef.
    ; Identify the baseline farm gate price and production per capita, plus the current production per capita. All
    ; cattle are priced the same per kg meat.
    let baseline-fgp filter [ i -> price-baseline-year = item 0 i ] fgp-beef-cow-carcass
    set baseline-fgp item 1 item 0 baseline-fgp
    let baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-beef
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    let current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-beef
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    let next-fgp baseline-fgp + (baseline-fgp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new farm gate price to the fgp list for implementation next year.
    set fgp-other-cattle-carcass lput list (Year + 1) (next-fgp) fgp-other-cattle-carcass
    set fgp-beef-cow-carcass lput list (Year + 1) (next-fgp) fgp-beef-cow-carcass
    set fgp-dairy-cow-carcass lput list (Year + 1) (next-fgp) fgp-dairy-cow-carcass

    ; Pork.
    ; Identify the baseline farm gate price and production per capita, plus the current production per capita.
    set baseline-fgp filter [ i -> price-baseline-year = item 0 i ] fgp-pig-carcass
    set baseline-fgp item 1 item 0 baseline-fgp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-pork
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-pork
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-fgp baseline-fgp + (baseline-fgp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new farm gate price to the fgp list for implementation next year.
    set fgp-pig-carcass lput list (Year + 1) (next-fgp) fgp-pig-carcass

    ; Lamb.
    ; Identify the baseline farm gate price and production per capita, plus the current production per capita.
    set baseline-fgp filter [ i -> price-baseline-year = item 0 i ] fgp-sheep-carcass
    set baseline-fgp item 1 item 0 baseline-fgp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-lamb
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-lamb
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-fgp baseline-fgp + (baseline-fgp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new farm gate price to the fgp list for implementation next year.
    set fgp-sheep-carcass lput list (Year + 1) (next-fgp) fgp-sheep-carcass

    ; Chicken.
    ; Identify the baseline farm gate price and production per capita, plus the current production per capita.
    set baseline-fgp filter [ i -> price-baseline-year = item 0 i ] fgp-broiler-carcass
    set baseline-fgp item 1 item 0 baseline-fgp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-chicken
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-chicken
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-fgp baseline-fgp + (baseline-fgp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new farm gate price to the fgp list for implementation next year.
    set fgp-broiler-carcass lput list (Year + 1) (next-fgp) fgp-broiler-carcass

    ; Eggs.
    ; Identify the baseline farm gate price and production per capita, plus the current production per capita.
    set baseline-fgp filter [ i -> price-baseline-year = item 0 i ] fgp-eggs
    set baseline-fgp item 1 item 0 baseline-fgp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-eggs
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-eggs
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-fgp baseline-fgp + (baseline-fgp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new farm gate price to the fgp list for implementation next year.
    set fgp-eggs lput list (Year + 1) (next-fgp) fgp-eggs

    ; Wool.
    ; Identify the baseline farm gate price and production per capita, plus the current production per capita.
    set baseline-fgp filter [ i -> price-baseline-year = item 0 i ] fgp-wool
    set baseline-fgp item 1 item 0 baseline-fgp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-wool
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-wool
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-fgp baseline-fgp + (baseline-fgp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new farm gate price to the fgp list for implementation next year.
    set fgp-wool lput list (Year + 1) (next-fgp) fgp-wool

    ; Raw milk.
    ; Identify the baseline farm gate price and production per capita, plus the current production per capita.
    set baseline-fgp filter [ i -> price-baseline-year = item 0 i ] fgp-raw-milk
    set baseline-fgp item 1 item 0 baseline-fgp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-rawmilk
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-rawmilk
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-fgp baseline-fgp + (baseline-fgp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new farm gate price to the fgp list for implementation next year.
    set fgp-raw-milk lput list (Year + 1) (next-fgp) fgp-raw-milk
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Assess whether to construct cultured meat production facilities ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Each year, one new cultured meat facility can be built.
  if sim-cm? = TRUE and year >= cm-init-yr [
    ; For each type of meat, check whether biosynthetic production is profitable at
    ; current prices and whether building a new factory would result in a breach
    ; in the biosynthetic market share cap for that particular meat. CM costs decline
    ; as total annual production rises past thresholds set by certain thresholds,
    ; leading to modification of the `cm-cost-` values.
    ; Beef.
    let facility-allowed-beef? TRUE
    let current-beef-profit-margin current-fgp-cattle-carcass - (cm-cost-beef * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk))
    if current-beef-profit-margin < 0 [
      set facility-allowed-beef? FALSE
    ]
    let potential-cm-production-beef cm-factory-capacity + (count cm-factories with [ meat-type = "Beef" ]  * cm-factory-capacity)
    if (potential-cm-production-beef / total-consumption-beef) * 100 > cm-max-share [
      set facility-allowed-beef? FALSE
    ]
    ; Pork.
    let facility-allowed-pork? TRUE
    let current-pork-profit-margin current-fgp-pig-carcass - (cm-cost-pork * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk))
    if current-pork-profit-margin < 0 [
      set facility-allowed-pork? FALSE
    ]
    let potential-cm-production-pork cm-factory-capacity + (count cm-factories with [ meat-type = "Pork" ]  * cm-factory-capacity)
    if (potential-cm-production-pork / total-consumption-pork) * 100 > cm-max-share [
      set facility-allowed-pork? FALSE
    ]
    ; Lamb.
    let facility-allowed-lamb? TRUE
    let current-lamb-profit-margin current-fgp-sheep-carcass - (cm-cost-lamb * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk))
    if current-lamb-profit-margin < 0 [
      set facility-allowed-lamb? FALSE
    ]
    let potential-cm-production-lamb cm-factory-capacity + (count cm-factories with [ meat-type = "Lamb" ]  * cm-factory-capacity)
    if (potential-cm-production-lamb / total-consumption-lamb) * 100 > cm-max-share [
      set facility-allowed-lamb? FALSE
    ]
    ; Chicken.
    let facility-allowed-chicken? TRUE
    let current-chicken-profit-margin current-fgp-broiler-carcass - (cm-cost-chicken * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk))
    if current-chicken-profit-margin < 0 [
      set facility-allowed-chicken? FALSE
    ]
    let potential-cm-production-chicken cm-factory-capacity + (count cm-factories with [ meat-type = "Chicken" ]  * cm-factory-capacity)
    if (potential-cm-production-chicken / total-consumption-chicken) * 100 > cm-max-share [
      set facility-allowed-chicken? FALSE
    ]
    ; We check whether there are any candidate meats that merit expanded production. If there is more
    ; than one, we will rank them to determine which has the greatest profit margin at current prices
    ; and build a facility for production of that meat.
    if facility-allowed-beef? = TRUE or facility-allowed-pork? = TRUE or facility-allowed-lamb? = TRUE or facility-allowed-chicken? = TRUE [
      ; Create the list of candidate meats.
      let candidate-meats []
      if facility-allowed-beef? = TRUE [
        set candidate-meats lput list (current-beef-profit-margin) ("Beef") candidate-meats
      ]
      if facility-allowed-pork? = TRUE [
        set candidate-meats lput list (current-pork-profit-margin) ("Pork") candidate-meats
      ]
      if facility-allowed-lamb? = TRUE [
        set candidate-meats lput list (current-lamb-profit-margin) ("Lamb") candidate-meats
      ]
      if facility-allowed-chicken? = TRUE [
        set candidate-meats lput list (current-chicken-profit-margin) ("Chicken") candidate-meats
      ]
      ; Order the list by profit margin.
      set candidate-meats sort-by [[list1 list2] -> first list1 < first list2] candidate-meats
      ; Create a cultured meat factory producing whichever meat has the greatest profit margin.
      create-cm-factories 1 [
        set meat-type item 1 item 0 candidate-meats
        set wholesale-stock 0
        set processed-meat []
        ; Specify aesthetics.
        if meat-type = "Beef" [
          set ycor -105
          set xcor 220 - (20 * count cm-factories with [ meat-type = "Beef" ])
        ]
        if meat-type = "Pork" [
          set ycor -120
          set xcor 220 - (20 * count cm-factories with [ meat-type = "Pork" ])
        ]
        if meat-type = "Lamb" [
          set ycor -135
          set xcor 220 - (20 * count cm-factories with [ meat-type = "Lamb" ])
        ]
        if meat-type = "Chicken" [
          set ycor -250
          set xcor 220 - (20 * count cm-factories with [ meat-type = "Chicken" ])
        ]
        set color [204 116 94]
        set shape "biosynth-factory"
        set size 20
      ]
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Assess whether to construct biosynthetic liquid production facilities ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Each year, one new egg and one new milk biosynthetic liquid facility can be built.
  if sim-pf? = TRUE and year >= pf-init-yr [
    ; For each type of biosynthetic liquid, check whether biosynthetic production is profitable at
    ; current prices and whether building a new factory would result in a breach in the biosynthetic
    ; market share cap for that particular product. BL costs decline as total annual production rises
    ; past thresholds set by certain thresholds, leading to modification of the `pf-cost-` values.
    ; Milk.
    let facility-allowed-milk? TRUE
    let current-milk-profit-margin current-fgp-raw-milk - (pf-cost-dairy * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk))
    if current-milk-profit-margin < 0 [
      set facility-allowed-milk? FALSE
    ]
    ; We calculate the proportion of total dairy consumption in raw milk equivalent that would be
    ; made up of biosynthetic production if another facility is added.
    let potential-pf-production-dairy pf-factory-dairy-capacity + (count pf-factories with [ product-type = "Dairy" ]  * pf-factory-dairy-capacity)
    let total-consumption-rawmilk (total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335000) + (total-consumption-butter * 0.2522000) + (total-consumption-cheese * 6.9140000)
    if (potential-pf-production-dairy / total-consumption-rawmilk) * 100 > pf-max-share [
      set facility-allowed-milk? FALSE
    ]
    ; If milk facilities are allowed, we will build one.
    if facility-allowed-milk? = TRUE [
      ; Create a biosynthetic liquid factory producing milk and converting it into dairy products.
      create-pf-factories 1 [
        set product-type "Dairy"
        set wholesale-stock-milk-cream 0
        set wholesale-stock-yoghurt 0
        set wholesale-stock-butter 0
        set wholesale-stock-cheese 0
        set wholesale-stock-eggs 0
        set processed-rawmilk []
        set processed-eggs []
        ; Specify aesthetics.
        if product-type = "Dairy" [
          set ycor -285
          set xcor 220 - (20 * count pf-factories with [ product-type = "Dairy" ])
        ]
        set color [239 213 167]
        set shape "biosynth-factory"
        set size 20
      ]
    ]
    ; Eggs.
    let facility-allowed-eggs? TRUE
    let current-egg-profit-margin current-fgp-eggs - (pf-cost-eggs * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk))
    if current-egg-profit-margin < 0 [
      set facility-allowed-eggs? FALSE
    ]
    let potential-pf-production-eggs pf-factory-egg-capacity + (count pf-factories with [ product-type = "Eggs" ]  * pf-factory-egg-capacity)
    if (potential-pf-production-eggs / total-consumption-eggs) * 100 > pf-max-share [
      set facility-allowed-eggs? FALSE
    ]
    ; If egg facilities are allowed, we will build one.
    if facility-allowed-eggs? = TRUE [
      ; Create a biosynthetic liquid factory producing egg.
      create-pf-factories 1 [
        set product-type "Eggs"
        set wholesale-stock-milk-cream 0
        set wholesale-stock-yoghurt 0
        set wholesale-stock-butter 0
        set wholesale-stock-cheese 0
        set wholesale-stock-eggs 0
        set processed-rawmilk []
        set processed-eggs []
        ; Specify aesthetics.
        if product-type = "Eggs" [
          set ycor -300
          set xcor 220 - (20 * count pf-factories with [ product-type = "Eggs" ])
        ]
        set color [239 213 167]
        set shape "biosynth-factory"
        set size 20
      ]
    ]
  ]

  ;;;;;;;;;
  ; Other ;
  ;;;;;;;;;

  update-agent-visibility
  ; Advance a year.
  set year year + 1
  ; Check if we have reached the specified end yr for the simulation.
  if year = sim-end-yr [
    stop
  ]
  tick
  repeat 3 [ beep wait 0.1 ]
end

to update-agent-visibility
  ifelse hide-farms? = TRUE [
    ask farms [ set hidden? TRUE ] ] [
    ask farms [ set hidden? FALSE ]
  ]
  ifelse hide-kommuner? = TRUE [
    ask kommuner [ set hidden? TRUE ] ] [
    ask kommuner [ set hidden? FALSE ]
  ]
  ifelse hide-fylker? = TRUE [
    ask fylker [ set hidden? TRUE ] ] [
    ask fylker [ set hidden? FALSE ]
  ]
  ifelse hide-slaughterhouses? = TRUE [
    ask slaughterhouses [ set hidden? TRUE ] ] [
    ask slaughterhouses [ set hidden? FALSE ]
  ]
  ifelse hide-farm-slaught-links? = TRUE [
    ask farm-slaughterhouse-links [ set hidden? TRUE ] ] [
    ask farm-slaughterhouse-links [ set hidden? FALSE ]
  ]
  ifelse hide-dairies? = TRUE [
    ask dairies [ set hidden? TRUE ] ] [
    ask dairies [ set hidden? FALSE ]
  ]
  ifelse hide-checkpoints? = TRUE [
    ask checkpoints [ set hidden? TRUE ] ] [
    ask checkpoints [ set hidden? FALSE ]
  ]
  ifelse hide-farm-dairy-links? = TRUE [
    ask farm-dairy-links [ set hidden? TRUE ] ] [
    ask farm-dairy-links [ set hidden? FALSE ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; REPORTERS                                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pair-distance-km [ turtle-a turtle-b ]
  ; Determine the NetLogo distance.
  let netlogo-distance [ distance turtle-a ] of turtle-b
  ; Multiply the the NetLogo distance by the dist-coeff calculated by the calibrate-distance procedure.
  let actual-distance netlogo-distance * dist-coeff
  report actual-distance
  ; Example 1: The following yields 161.4 km. This is the same as the actual distance.
  ;   show pair-distance-km (one-of dairies with [ dairy-name = "TINE Meieriet Sem" ]) (one-of dairies with [ dairy-name = "TINE Meieriet Setesdal" ])
  ; Example 2: The following yields 68.89 km. The actual is 68.95 km.
  ;   show pair-distance-km (one-of dairies with [ dairy-name = "Synnøve Finden Alvdal" ]) (one-of dairies with [ dairy-name = "TINE Meieriet Frya" ])
end

to-report total-farms
  report count farms with [ active? = TRUE ]
end

; Define a reporter that returns the present population of the specified kommune.
; Usage example: `ask kommune 342 [ print local-population ]`
to-report local-population
  ; Filter population list to the sublist for the relevant year.
  let my-population filter [ i -> year = item 0 i ] population
  ; Extract the population value for the relevant year.
  set my-population item 1 item 0 my-population
  report my-population
end

to-report total-population
  report sum [ local-population ] of kommuner
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report farm engagement in activities ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report total-grain-oilseed-farms
  report count farms with [ active? = TRUE and (ha-barley > 0 or ha-oats > 0 or ha-wheat > 0 or ha-rye-triticale > 0 or ha-oilseeds > 0) ]
end

to-report total-other-field-hort-farms
  report count farms with [ active? = TRUE and (ha-potatoes > 0 or ha-vegetables > 0 or ha-fodder-silage > 0 or ha-other-crops > 0 or ha-orchards > 0 or ha-berries > 0) ]
end

to-report total-livestock-farms
  report count farms with [ active? = TRUE and (num-dairy-cows >  0 or num-beef-cows >  0 or num-other-cattle >  0 or num-sheep >  0 or num-pigs >  0 or num-broilers >  0 or num-laying-hens >  0) ]
end

to-report total-cattle-farms
  report count farms with [ active? = TRUE and (num-dairy-cows >  0 or num-beef-cows >  0 or num-other-cattle >  0) ]
end

to-report total-dairy-farms
  report count farms with [ active? = TRUE and (num-dairy-cows >  0) ]
end

to-report total-sheep-farms
  report count farms with [ active? = TRUE and (num-sheep >  0) ]
end

to-report total-pig-farms
  report count farms with [ active? = TRUE and (num-pigs >  0) ]
end

to-report total-broiler-farms
  report count farms with [ active? = TRUE and (num-broilers >  0) ]
end

to-report total-laying-hen-farms
  report count farms with [ active? = TRUE and (num-laying-hens >  0) ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report farm activities ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report total-ha-barley
  report sum [ ha-barley ] of farms with [active? = TRUE]
end

to-report total-ha-oats
  report sum [ ha-oats ] of farms with [active? = TRUE]
end

to-report total-ha-wheat
  report sum [ ha-wheat ] of farms with [active? = TRUE]
end

to-report total-ha-rye-triticale
  report sum [ ha-rye-triticale ] of farms with [active? = TRUE]
end

to-report total-ha-oilseeds
  report sum [ ha-oilseeds ] of farms with [active? = TRUE]
end

to-report total-ha-potatoes
  report sum [ ha-potatoes ] of farms with [active? = TRUE]
end

to-report total-ha-vegetables
  report sum [ ha-vegetables ] of farms with [active? = TRUE]
end

to-report total-ha-fodder-silage
  report sum [ ha-fodder-silage ] of farms with [active? = TRUE]
end

to-report total-ha-other-crops
  report sum [ ha-other-crops ] of farms with [active? = TRUE]
end

to-report total-ha-orchards
  report sum [ ha-orchards ] of farms with [active? = TRUE]
end

to-report total-ha-berries
  report sum [ ha-berries ] of farms with [active? = TRUE]
end

to-report total-dairy-cows
  report sum [ num-dairy-cows ] of farms with [active? = TRUE]
end

to-report total-beef-cows
  report sum [ num-beef-cows ] of farms with [active? = TRUE]
end

to-report total-other-cattle
  report sum [ num-other-cattle ] of farms with [active? = TRUE]
end

to-report total-sheep
  report sum [ num-sheep ] of farms with [active? = TRUE]
end

to-report total-pigs
  report sum [ num-pigs ] of farms with [active? = TRUE]
end

to-report total-broilers
  report sum [ num-broilers ] of farms with [active? = TRUE]
end

to-report total-laying-hens
  report sum [ num-laying-hens ] of farms with [active? = TRUE]
end

to-report total-ha-grain-oilseed
  report (sum [ ha-barley ] of farms with [active? = TRUE] + sum [ ha-oats ] of farms with [active? = TRUE] + sum [ ha-wheat ] of farms with [active? = TRUE] + sum [ ha-rye-triticale ] of farms with [active? = TRUE] + sum [ ha-oilseeds ] of farms with [active? = TRUE])
end

to-report total-ha-other-field-hort
  report (sum [ ha-potatoes ] of farms with [active? = TRUE] + sum [ ha-vegetables ] of farms with [active? = TRUE] + sum [ ha-fodder-silage ] of farms with [active? = TRUE] + sum [ ha-other-crops ] of farms with [active? = TRUE] + sum [ ha-orchards ] of farms with [active? = TRUE] + sum [ ha-berries ] of farms with [active? = TRUE])
end

to-report total-livestock
  report (sum [ num-beef-cows ] of farms with [active? = TRUE] + sum [ num-other-cattle ] of farms with [active? = TRUE] + sum [ num-dairy-cows ] of farms with [active? = TRUE] +  sum [ num-sheep ] of farms with [active? = TRUE] + sum [ num-pigs ] of farms with [active? = TRUE] + sum [ num-broilers ] of farms with [active? = TRUE] + sum [ num-laying-hens ] of farms with [active? = TRUE])
end

to-report total-cattle
  report (sum [ num-beef-cows ] of farms with [active? = TRUE] + sum [ num-other-cattle ] of farms with [active? = TRUE] + sum [ num-dairy-cows ] of farms with [active? = TRUE])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report farm-production ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report farm-production-barley
  let my-production 0
  if ha-barley > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-barley
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-barley
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-oats
  let my-production 0
  if ha-oats > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-oats
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-oats
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-wheat
  let my-production 0
  if ha-wheat > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-wheat
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-wheat
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-rye-triticale
  let my-production 0
  if ha-rye-triticale > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-rye-triticale
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-rye-triticale
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-oilseeds
  let my-production 0
  if ha-oilseeds > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-oilseeds
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-oilseeds
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-potatoes
  let my-production 0
  if ha-potatoes > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-potatoes
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-potatoes
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-vegetables
  let my-production 0
  if ha-vegetables > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-vegetables
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-vegetables
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-fodder-silage
  let my-production 0
  if ha-fodder-silage > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-fodder-silage
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-fodder-silage
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-other-crops
  let my-production 0
  if ha-other-crops > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-other-crops
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-other-crops
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-orchards
  let my-production 0
  if ha-orchards > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-orchards
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-orchards
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-berries
  let my-production 0
  if ha-berries > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-berries
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * ha-berries
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-beef
  let my-production 0
  if num-dairy-cows + num-beef-cows + num-other-cattle > 0 [
    let my-fylke-id fylke-id
    let local-yield-dairy-cows 0
    let local-yield-beef-cows 0
    let local-yield-other-cattle 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield-dairy-cows filter [ i -> Year = item 0 i ] yield-dairy-cow-carcass
      set local-yield-dairy-cows item 1 item 0 local-yield-dairy-cows
      set local-yield-beef-cows filter [ i -> Year = item 0 i ] yield-beef-cow-carcass
      set local-yield-beef-cows item 1 item 0 local-yield-beef-cows
      set local-yield-other-cattle filter [ i -> Year = item 0 i ] yield-other-cattle-carcass
      set local-yield-other-cattle item 1 item 0 local-yield-other-cattle
    ]
    set my-production (local-yield-dairy-cows * num-dairy-cows) + (local-yield-beef-cows * num-beef-cows) + (local-yield-other-cattle * num-other-cattle)
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-pork
  let my-production 0
  if num-pigs > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-pig-carcass
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-pigs
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-lamb
  let my-production 0
  if num-sheep > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-sheep-carcass
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-sheep
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-chicken
  let my-production 0
  if num-broilers > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-broiler-carcass
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-broilers
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-eggs
  let my-production 0
  if num-laying-hens > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-eggs
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-laying-hens
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-wool
  let my-production 0
  if num-sheep > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-wool
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-sheep
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report farm-production-raw-milk
  let my-production 0
  if num-dairy-cows > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-raw-milk
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-dairy-cows
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report my-production
end

to-report total-farm-production-barley
  report sum [ farm-production-barley ] of farms with [active? = TRUE]
end

to-report total-farm-production-oats
  report sum [ farm-production-oats ] of farms with [active? = TRUE]
end

to-report total-farm-production-wheat
  report sum [ farm-production-wheat ] of farms with [active? = TRUE]
end

to-report total-farm-production-rye-triticale
  report sum [ farm-production-rye-triticale ] of farms with [active? = TRUE]
end

to-report total-farm-production-oilseeds
  report sum [ farm-production-oilseeds ] of farms with [active? = TRUE]
end

to-report total-farm-production-potatoes
  report sum [ farm-production-potatoes ] of farms with [active? = TRUE]
end

to-report total-farm-production-vegetables
  report sum [ farm-production-vegetables ] of farms with [active? = TRUE]
end

to-report total-farm-production-fodder-silage
  report sum [ farm-production-fodder-silage ] of farms with [active? = TRUE]
end

to-report total-farm-production-other-crops
  report sum [ farm-production-other-crops ] of farms with [active? = TRUE]
end

to-report total-farm-production-orchards
  report sum [ farm-production-orchards ] of farms with [active? = TRUE]
end

to-report total-farm-production-berries
  report sum [ farm-production-berries ] of farms with [active? = TRUE]
end

to-report total-farm-production-beef
  report sum [ farm-production-beef ] of farms with [active? = TRUE]
end

to-report total-farm-production-lamb
  report sum [ farm-production-lamb ] of farms with [active? = TRUE]
end

to-report total-farm-production-pork
  report sum [ farm-production-pork ] of farms with [active? = TRUE]
end

to-report total-farm-production-chicken
  report sum [ farm-production-chicken ] of farms with [active? = TRUE]
end

to-report total-farm-production-eggs
  report sum [ farm-production-eggs ] of farms with [active? = TRUE]
end

to-report total-farm-production-wool
  report sum [ farm-production-wool ] of farms with [active? = TRUE]
end

to-report total-farm-production-raw-milk
  report sum [ farm-production-raw-milk ] of farms with [active? = TRUE]
end

to-report total-farm-production-animal-products
  report total-farm-production-beef + total-farm-production-lamb + total-farm-production-pork + total-farm-production-chicken + total-farm-production-eggs + total-farm-production-wool + total-farm-production-raw-milk
end

to-report total-farm-production-grain-oilseeds
  report sum [ farm-production-barley ] of farms with [active? = TRUE] + sum [ farm-production-oats ] of farms with [active? = TRUE] + sum [ farm-production-wheat ] of farms with [active? = TRUE] + sum [ farm-production-rye-triticale ] of farms with [active? = TRUE] + sum [ farm-production-oilseeds ] of farms with [active? = TRUE]
end

to-report total-farm-production-all-other-crops
  report sum [ farm-production-potatoes ] of farms with [active? = TRUE] + sum [ farm-production-vegetables ] of farms with [active? = TRUE] + sum [ farm-production-fodder-silage ] of farms with [active? = TRUE] + sum [ farm-production-other-crops ] of farms with [active? = TRUE] + sum [ farm-production-orchards ] of farms with [active? = TRUE] + sum [ farm-production-berries ] of farms with [active? = TRUE]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report factory production ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report total-factory-production-beef
  report count cm-factories with [ meat-type = "Beef" ] * cm-factory-capacity
end

to-report total-factory-production-pork
  report count cm-factories with [ meat-type = "Pork" ] * cm-factory-capacity
end

to-report total-factory-production-lamb
  report count cm-factories with [ meat-type = "Lamb" ] * cm-factory-capacity
end

to-report total-factory-production-chicken
  report count cm-factories with [ meat-type = "Chicken" ] * cm-factory-capacity
end

to-report total-factory-production-meat
  report total-factory-production-beef + total-factory-production-pork + total-factory-production-lamb + total-factory-production-chicken
end

to-report total-factory-production-eggs
  report count pf-factories with [ product-type = "Eggs" ] * pf-factory-egg-capacity
end

to-report total-factory-production-raw-milk
  report count pf-factories with [ product-type = "Dairy" ] * pf-factory-dairy-capacity
end

to-report total-factory-production-pf
  report total-factory-production-eggs + total-factory-production-raw-milk
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report farm-gate price ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report current-fgp-barley
  let national-fgp-barley filter [ i -> Year = item 0 i ] fgp-barley
  set national-fgp-barley item 1 item 0 national-fgp-barley
  report national-fgp-barley
end

to-report current-fgp-oats
  let national-fgp-oats filter [ i -> Year = item 0 i ] fgp-oats
  set national-fgp-oats item 1 item 0 national-fgp-oats
  report national-fgp-oats
end

to-report current-fgp-wheat
  let national-fgp-wheat filter [ i -> Year = item 0 i ] fgp-wheat
  set national-fgp-wheat item 1 item 0 national-fgp-wheat
  report national-fgp-wheat
end

to-report current-fgp-rye-triticale
  let national-fgp-rye-triticale filter [ i -> Year = item 0 i ] fgp-rye-triticale
  set national-fgp-rye-triticale item 1 item 0 national-fgp-rye-triticale
  report national-fgp-rye-triticale
end

to-report current-fgp-oilseeds
  let national-fgp-oilseeds filter [ i -> Year = item 0 i ] fgp-oilseeds
  set national-fgp-oilseeds item 1 item 0 national-fgp-oilseeds
  report national-fgp-oilseeds
end

to-report current-fgp-potatoes
  let national-fgp-potatoes filter [ i -> Year = item 0 i ] fgp-potatoes
  set national-fgp-potatoes item 1 item 0 national-fgp-potatoes
  report national-fgp-potatoes
end

to-report current-fgp-vegetables
  let national-fgp-vegetables filter [ i -> Year = item 0 i ] fgp-vegetables
  set national-fgp-vegetables item 1 item 0 national-fgp-vegetables
  report national-fgp-vegetables
end

to-report current-fgp-fodder-silage
  let national-fgp-fodder-silage filter [ i -> Year = item 0 i ] fgp-fodder-silage
  set national-fgp-fodder-silage item 1 item 0 national-fgp-fodder-silage
  report national-fgp-fodder-silage
end

to-report current-fgp-other-crops
  let national-fgp-other-crops filter [ i -> Year = item 0 i ] fgp-other-crops
  set national-fgp-other-crops item 1 item 0 national-fgp-other-crops
  report national-fgp-other-crops
end

to-report current-fgp-pome-stone-fruit
  let national-fgp-pome-stone-fruit filter [ i -> Year = item 0 i ] fgp-pome-stone-fruit
  set national-fgp-pome-stone-fruit item 1 item 0 national-fgp-pome-stone-fruit
  report national-fgp-pome-stone-fruit
end

to-report current-fgp-berries
  let national-fgp-berries filter [ i -> Year = item 0 i ] fgp-berries
  set national-fgp-berries item 1 item 0 national-fgp-berries
  report national-fgp-berries
end

to-report current-fgp-cattle-carcass
  let national-fgp-cattle filter [ i -> Year = item 0 i ] fgp-other-cattle-carcass
  set national-fgp-cattle item 1 item 0 national-fgp-cattle
  report national-fgp-cattle
end

to-report current-fgp-pig-carcass
  let national-fgp-pigs filter [ i -> Year = item 0 i ] fgp-pig-carcass
  set national-fgp-pigs item 1 item 0 national-fgp-pigs
  report national-fgp-pigs
end

to-report current-fgp-sheep-carcass
  let national-fgp-sheep filter [ i -> Year = item 0 i ] fgp-sheep-carcass
  set national-fgp-sheep item 1 item 0 national-fgp-sheep
  report national-fgp-sheep
end

to-report current-fgp-broiler-carcass
  let national-fgp-broiler filter [ i -> Year = item 0 i ] fgp-broiler-carcass
  set national-fgp-broiler item 1 item 0 national-fgp-broiler
  report national-fgp-broiler
end

to-report current-fgp-eggs
  let national-fgp-eggs filter [ i -> Year = item 0 i ] fgp-eggs
  set national-fgp-eggs item 1 item 0 national-fgp-eggs
  report national-fgp-eggs
end

to-report current-fgp-raw-milk
  let national-fgp-raw-milk filter [ i -> Year = item 0 i ] fgp-raw-milk
  set national-fgp-raw-milk item 1 item 0 national-fgp-raw-milk
  report national-fgp-raw-milk
end

to-report current-fgp-wool
  let national-fgp-wool filter [ i -> Year = item 0 i ] fgp-wool
  set national-fgp-wool item 1 item 0 national-fgp-wool
  report national-fgp-wool
end

;;;;;;;;;;;;;;;;;;;;;;;;;
; Calculate farm income ;
;;;;;;;;;;;;;;;;;;;;;;;;;

; For animal products, send the production to a dairy or slaughterhouse. Initially use the nearest facility, but update this later to account for capacity.
; Calculate the mean income each year for the first five years, updating it for each of these years, making this the baseline against which farms judge viability.
to-report farm-income
  let income-barley (farm-production-barley * current-fgp-barley)
  let income-oats (farm-production-oats * current-fgp-oats)
  let income-wheat (farm-production-wheat * current-fgp-wheat)
  let income-rye-triticale (farm-production-rye-triticale * current-fgp-rye-triticale)
  let income-oilseeds (farm-production-oilseeds * current-fgp-oilseeds)
  let income-potatoes (farm-production-potatoes * current-fgp-potatoes)
  let income-vegetables (farm-production-vegetables * current-fgp-vegetables)
  let income-fodder-silage (farm-production-fodder-silage * current-fgp-fodder-silage)
  let income-other-crops (farm-production-other-crops * current-fgp-other-crops)
  let income-pome-stone-fruit (farm-production-orchards * current-fgp-pome-stone-fruit)
  let income-berries (farm-production-berries * current-fgp-berries)
  let income-beef (farm-production-beef * current-fgp-cattle-carcass)
  let income-pork (farm-production-pork * current-fgp-pig-carcass)
  let income-lamb (farm-production-lamb * current-fgp-sheep-carcass)
  let income-chicken (farm-production-chicken * current-fgp-broiler-carcass)
  let income-eggs (farm-production-eggs * current-fgp-eggs)
  let income-wool (farm-production-wool * current-fgp-wool)
  let income-raw-milk (farm-production-raw-milk * current-fgp-raw-milk)
  let subsidy-income (subsidy-nonlivestock + subsidy-livestock + subsidy-milk)
  let total-farm-income (income-barley + income-oats + income-wheat + income-rye-triticale + income-oilseeds + income-potatoes + income-vegetables + income-fodder-silage + income-other-crops + income-pome-stone-fruit + income-berries + income-beef + income-lamb + income-pork + income-chicken + income-eggs + income-wool + income-raw-milk + subsidy-income)
  if  active? = FALSE [ set total-farm-income 0 ]
  report total-farm-income
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report raw milk division rations ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report raw-milk-division-ratio
  ; Here, we calculate how raw milk should be divided between the various
  ; dairy products given demand. We assume they are valued equally so do
  ; not prioritise production of one above any other. This is essentially
  ; determined by the conversion factors involved in transforming the
  ; raw milk to the consumer products.
  ; Calculate the toal milk required for each dairy product in current year.
  let raw-milk-required-milk-cream total-consumption-milk-cream * 1
  let raw-milk-required-yoghurt total-consumption-yoghurt * 1.13
  let raw-milk-required-butter total-consumption-butter * 0.252
  let raw-milk-required-cheese total-consumption-cheese * 6.91
  let total-raw-milk-required (raw-milk-required-milk-cream + raw-milk-required-yoghurt + raw-milk-required-butter + raw-milk-required-cheese)
  ; Calculate the share of raw milk that goes into each product.
  let coeff-milk-cream raw-milk-required-milk-cream / total-raw-milk-required
  let coeff-yoghurt raw-milk-required-yoghurt / total-raw-milk-required
  let coeff-butter raw-milk-required-butter / total-raw-milk-required
  let coeff-cheese raw-milk-required-cheese / total-raw-milk-required
  ; Create a list that contains the coefficients and report it.
  let dairy-coeff (list coeff-milk-cream coeff-yoghurt coeff-butter coeff-cheese )
  report dairy-coeff
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report total imports and exports ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report current-imports-beef
  let total-imports-beef-yr filter [ i -> (Year - 1) = item 0 i ] total-imports-beef
  set total-imports-beef-yr item 1 item 0 total-imports-beef-yr
  report total-imports-beef-yr
end

to-report current-exports-beef
  let total-exports-beef-yr filter [ i -> (Year - 1) = item 0 i ] total-exports-beef
  set total-exports-beef-yr item 1 item 0 total-exports-beef-yr
  report total-exports-beef-yr
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report per capita production of product in current year ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report per-capita-production-beef
  ; Calculate current net imports.
  let current-imports filter [ i -> Year = item 0 i ] total-imports-beef
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-beef
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Deduct net imports from domestic consumption to determine domestic production.
  let domestic-production total-consumption-beef - current-net-imports
  ; We then divide the total domestic production by the current population.
  let per-capita-domestic-production domestic-production / total-population
  report per-capita-domestic-production
end

to-report per-capita-production-pork
  ; Calculate current net imports.
  let current-imports filter [ i -> Year = item 0 i ] total-imports-pork
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-pork
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Deduct net imports from domestic consumption to determine domestic production.
  let domestic-production total-consumption-pork - current-net-imports
  ; We then divide the total domestic production by the current population.
  let per-capita-domestic-production domestic-production / total-population
  report per-capita-domestic-production
end

to-report per-capita-production-lamb
  ; Calculate current net imports.
  let current-imports filter [ i -> Year = item 0 i ] total-imports-lamb
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-lamb
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Deduct net imports from domestic consumption to determine domestic production.
  let domestic-production total-consumption-lamb - current-net-imports
  ; We then divide the total domestic production by the current population.
  let per-capita-domestic-production domestic-production / total-population
  report per-capita-domestic-production
end

to-report per-capita-production-chicken
  ; Calculate current net imports.
  let current-imports filter [ i -> Year = item 0 i ] total-imports-chicken
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-chicken
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Deduct net imports from domestic consumption to determine domestic production.
  let domestic-production total-consumption-chicken - current-net-imports
  ; We then divide the total domestic production by the current population.
  let per-capita-domestic-production domestic-production / total-population
  report per-capita-domestic-production
end

to-report per-capita-production-eggs
  ; Calculate current net imports.
  let current-imports filter [ i -> Year = item 0 i ] total-imports-eggs
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-eggs
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Deduct net imports from domestic consumption to determine domestic production.
  let domestic-production total-consumption-eggs - current-net-imports
  ; We then divide the total domestic production by the current population.
  let per-capita-domestic-production domestic-production / total-population
  report per-capita-domestic-production
end

to-report per-capita-production-wool
  ; No trade is done in wool so the per capita production is simply the domestic production
  ; divided by the current population.
  let per-capita-domestic-production total-farm-production-wool / total-population
  report per-capita-domestic-production
end

to-report per-capita-production-rawmilk
  ; Calculate current net imports of milk and cream.
  let current-imports filter [ i -> Year = item 0 i ] total-imports-milk-cream
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-milk-cream
  set current-exports item 1 item 0 current-exports
  let current-net-imports-milk-cream current-imports - current-exports
  ; Calculate current net imports of yoghurt.
  set current-imports filter [ i -> Year = item 0 i ] total-imports-yoghurt
  set current-imports item 1 item 0 current-imports
  set current-exports filter [ i -> Year = item 0 i ] total-exports-yoghurt
  set current-exports item 1 item 0 current-exports
  let current-net-imports-yoghurt current-imports - current-exports
  ; Calculate current net imports of butter.
  set current-imports filter [ i -> Year = item 0 i ] total-imports-butter
  set current-imports item 1 item 0 current-imports
  set current-exports filter [ i -> Year = item 0 i ] total-exports-butter
  set current-exports item 1 item 0 current-exports
  let current-net-imports-butter current-imports - current-exports
  ; Calculate current net imports of cheese.
  set current-imports filter [ i -> Year = item 0 i ] total-imports-cheese
  set current-imports item 1 item 0 current-imports
  set current-exports filter [ i -> Year = item 0 i ] total-exports-cheese
  set current-exports item 1 item 0 current-exports
  let current-net-imports-cheese current-imports - current-exports
  ; Deduct net imports from domestic consumption to determine domestic production.
  let domestic-production-milk-cream total-consumption-milk-cream - current-net-imports-milk-cream
  let domestic-production-yoghurt total-consumption-yoghurt - current-net-imports-yoghurt
  let domestic-production-butter total-consumption-butter - current-net-imports-butter
  let domestic-production-cheese total-consumption-cheese - current-net-imports-cheese
  ; Convert domestic production figures into raw milk equivalent and aggregate.
  let domestic-production-rawmilk (domestic-production-milk-cream * 1) + (domestic-production-yoghurt * 1.1335000) + (domestic-production-butter * 0.2522000) + (domestic-production-cheese * 6.9140000)
  ; We then divide the total domestic production by the current population.
  let per-capita-domestic-production domestic-production-rawmilk / total-population
  report per-capita-domestic-production
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report total CO2-equiv. emissions by activity in kt                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report total-emissions-barley
  report (total-ha-barley * emissions-ha-barley) / 1000000
end

to-report total-emissions-oats
  report (total-ha-oats * emissions-ha-oats) / 1000000
end

to-report total-emissions-wheat
  report (total-ha-wheat * emissions-ha-wheat) / 1000000
end

to-report total-emissions-rye-triticale
  report (total-ha-rye-triticale * emissions-ha-rye-triticale) / 1000000
end

to-report total-emissions-oilseeds
  report (total-ha-oilseeds * emissions-ha-oilseeds) / 1000000
end

to-report total-emissions-potatoes
  report (total-ha-potatoes * emissions-ha-potatoes) / 1000000
end

to-report total-emissions-vegetables
  report (total-ha-vegetables * emissions-ha-vegetables) / 1000000
end

to-report total-emissions-fodder-silage
  report (total-ha-fodder-silage * emissions-ha-fodder-silage) / 1000000
end

to-report total-emissions-other-crops
  report (total-ha-other-crops * emissions-ha-other-crops) / 1000000
end

to-report total-emissions-orchards
  report (total-ha-orchards * emissions-ha-orchards) / 1000000
end

to-report total-emissions-berries
  report (total-ha-berries * emissions-ha-berries) / 1000000
end

to-report total-emissions-dairy-cows
  report (total-dairy-cows * emissions-head-dairy-cows) / 1000000
end

to-report total-emissions-beef-cows
  report (total-beef-cows * emissions-head-beef-cows) / 1000000
end

to-report total-emissions-other-cattle
  report (total-other-cattle * emissions-head-other-cattle) / 1000000
end

to-report total-emissions-sheep
  report (total-sheep * emissions-head-sheep) / 1000000
end

to-report total-emissions-pigs
  report (total-pigs * emissions-head-pigs) / 1000000
end

to-report total-emissions-broilers
  report (total-broilers * emissions-head-broilers) / 1000000
end

to-report total-emissions-laying-hens
  report (total-laying-hens * emissions-head-laying-hens) / 1000000
end

to-report total-emissions-crops
  report total-emissions-barley + total-emissions-oats + total-emissions-wheat + total-emissions-rye-triticale + total-emissions-oilseeds + total-emissions-potatoes + total-emissions-vegetables + total-emissions-fodder-silage + total-emissions-other-crops + total-emissions-orchards + total-emissions-berries
end

to-report total-emissions-livestock
  report total-emissions-dairy-cows + total-emissions-beef-cows + total-emissions-other-cattle + total-emissions-sheep + total-emissions-pigs + total-emissions-broilers + total-emissions-laying-hens
end

to-report total-emissions-agriculture
  report total-emissions-crops + total-emissions-livestock
end

; Report total emissions for the year from CM and PF production in kt.
to-report total-emissions-cm
  report sum [ factory-emissions ] of cm-factories / 1000000
end

to-report total-emissions-pf-dairy
  report sum [ factory-emissions ] of pf-factories with [ product-type = "Dairy" ] / 1000000
end

to-report total-emissions-pf-egg
  report sum [ factory-emissions ] of pf-factories with [ product-type = "Eggs" ] / 1000000
end

to-report total-emissions-cf
  report total-emissions-cm + total-emissions-pf-dairy + total-emissions-pf-egg
end

to-report total-emissions
  report total-emissions-agriculture + total-emissions-cf
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report percentage share of emissions by activity                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report emissions-share-barley
  report (total-emissions-barley / total-emissions) * 100
end

to-report emissions-share-oats
  report (total-emissions-oats / total-emissions) * 100
end

to-report emissions-share-wheat
  report (total-emissions-wheat / total-emissions) * 100
end

to-report emissions-share-rye-triticale
  report (total-emissions-rye-triticale / total-emissions) * 100
end

to-report emissions-share-oilseeds
  report (total-emissions-oilseeds / total-emissions) * 100
end

to-report emissions-share-potatoes
  report (total-emissions-potatoes / total-emissions) * 100
end

to-report emissions-share-vegetables
  report (total-emissions-vegetables / total-emissions) * 100
end

to-report emissions-share-fodder-silage
  report (total-emissions-fodder-silage / total-emissions) * 100
end

to-report emissions-share-other-crops
  report (total-emissions-other-crops / total-emissions) * 100
end

to-report emissions-share-orchards
  report (total-emissions-orchards / total-emissions) * 100
end

to-report emissions-share-berries
  report (total-emissions-berries / total-emissions) * 100
end

to-report emissions-share-dairy-cows
  report (total-emissions-dairy-cows / total-emissions) * 100
end

to-report emissions-share-beef-cows
  report (total-emissions-beef-cows / total-emissions) * 100
end

to-report emissions-share-other-cattle
  report (total-emissions-other-cattle / total-emissions) * 100
end

to-report emissions-share-sheep
  report (total-emissions-sheep / total-emissions) * 100
end

to-report emissions-share-pigs
  report (total-emissions-pigs / total-emissions) * 100
end

to-report emissions-share-broilers
  report (total-emissions-broilers / total-emissions) * 100
end

to-report emissions-share-laying-hens
  report (total-emissions-laying-hens / total-emissions) * 100
end

to-report emissions-share-crops
  report emissions-share-barley + emissions-share-oats + emissions-share-wheat + emissions-share-rye-triticale + emissions-share-oilseeds + emissions-share-potatoes + emissions-share-vegetables + emissions-share-fodder-silage + emissions-share-other-crops + emissions-share-orchards + emissions-share-berries
end

to-report emissions-share-livestock
  report emissions-share-dairy-cows + emissions-share-beef-cows + emissions-share-other-cattle + emissions-share-sheep + emissions-share-pigs + emissions-share-broilers + emissions-share-laying-hens
end

to-report emissions-share-cm
  report (total-emissions-cm / total-emissions) * 100
end

to-report emissions-share-pf-dairy
  report (total-emissions-pf-dairy / total-emissions) * 100
end

to-report emissions-share-pf-egg
  report (total-emissions-pf-egg / total-emissions) * 100
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Average emissions of domestically produced products by type                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Conventional dairy has an emissions coefficient of 1.139 kg CO2-eq. per kg.
; PF dairy has an emissions coefficient set by emissions-pf-dairy.
to-report av-emissions-kg-dairy
  ; Total emissions from farmed dairy in kg.
  let emissions-kg-conventional (total-farm-production-raw-milk * 1000) * 1.139
  ; Total emissions from PF dairy in kg.
  let emissions-kg-pf total-emissions-pf-dairy * 1000000
  ; Total production of dairy in kg.
  let total-production-kg (total-farm-production-raw-milk + (count pf-factories with [ product-type = "Dairy" ] * pf-factory-dairy-capacity)) * 1000
  ; Total combined emissions divided by total combined production.
  report (emissions-kg-conventional + emissions-kg-pf) / total-production-kg
end


; Conventional eggs have an emissions coefficient of 0.777 kg CO2-eq. per kg.
; PF eggs have an emissions coefficient set by emissions-pf-egg.
to-report av-emissions-kg-egg
  ; Total emissions from farmed eggs in kg.
  let emissions-kg-conventional (total-farm-production-eggs * 1000) * 0.777
  ; Total emissions from PF egg in kg.
  let emissions-kg-pf total-emissions-pf-egg * 1000000
  ; Total production of egg in kg.
  let total-production-kg (total-farm-production-eggs + (count pf-factories with [ product-type = "Eggs" ] * pf-factory-egg-capacity)) * 1000
  ; Total combined emissions divided by total combined production.
  report (emissions-kg-conventional + emissions-kg-pf) / total-production-kg
end


; TRY TO CALCULATE THE AVERAGE EMISSIONS PER KG OF DOMESTICALLY PRODUCED PROTEIN AS A HEADLINE FIGURE THAT CAN BE USED IN OUR PLOT!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report percentage change in product trade balance relative to previous year ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Beef.
to-report trade-balance-change-perc-beef
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-beef
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-beef
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-beef
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-beef
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Pork.
to-report trade-balance-change-perc-pork
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-pork
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-pork
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-pork
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-pork
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Lamb.
to-report trade-balance-change-perc-lamb
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-lamb
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-lamb
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-lamb
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-lamb
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Chicken.
to-report trade-balance-change-perc-chicken
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-chicken
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-chicken
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-chicken
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-chicken
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Eggs.
to-report trade-balance-change-perc-eggs
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-eggs
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-eggs
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-eggs
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-eggs
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Milk and cream.
to-report trade-balance-change-perc-milk-cream
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-milk-cream
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-milk-cream
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-milk-cream
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-milk-cream
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Yoghurt.
to-report trade-balance-change-perc-yoghurt
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-yoghurt
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-yoghurt
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-yoghurt
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-yoghurt
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Butter.
to-report trade-balance-change-perc-butter
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-butter
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-butter
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-butter
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-butter
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

; Cheese.
to-report trade-balance-change-perc-cheese
  ; Extract the imports and exports data for the current year, then calculate the net imports.
  let previous-imports filter [ i -> (Year - 1) = item 0 i ] total-imports-cheese
  set previous-imports item 1 item 0 previous-imports
  let previous-exports filter [ i -> (Year - 1) = item 0 i ] total-exports-cheese
  set previous-exports item 1 item 0 previous-exports
  let previous-net-imports previous-imports - previous-exports
  ; Calculate current net imports
  let current-imports filter [ i -> Year = item 0 i ] total-imports-cheese
  set current-imports item 1 item 0 current-imports
  let current-exports filter [ i -> Year = item 0 i ] total-exports-cheese
  set current-exports item 1 item 0 current-exports
  let current-net-imports current-imports - current-exports
  ; Calculate relative change in net imports (tells us percentage increase or decrease relative to previous year).
  let change-net-import ((previous-net-imports - current-net-imports) / previous-net-imports) * -100
  report change-net-import
end

;;;;;;;;;;;;;;;;;;;;;;
; Report consumption ;
;;;;;;;;;;;;;;;;;;;;;;

to-report local-consumption-beef
  ; Extract the kommune beef consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-beef
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-pork
  ; Extract the kommune pork consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-pork
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-lamb
  ; Extract the kommune lamb consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-lamb
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-chicken
  ; Extract the kommune chicken consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-chicken
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-eggs
  ; Extract the kommune egg consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-eggs
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-milk-cream
  ; Extract the kommune milk & cream consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-milk-cream
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-yoghurt
  ; Extract the kommune yoghurt consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-yoghurt
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-butter
  ; Extract the kommune butter consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-butter
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report local-consumption-cheese
  ; Extract the kommune cheese consumption for the current year.
  let my-consumption filter [ i -> Year = item 0 i ] consumption-cheese
  set my-consumption item 1 item 0 my-consumption
  report my-consumption
end

to-report total-consumption-beef
  report sum [ local-consumption-beef ] of kommuner
end

to-report total-consumption-pork
  report sum [ local-consumption-pork ] of kommuner
end

to-report total-consumption-lamb
  report sum [ local-consumption-lamb ] of kommuner
end

to-report total-consumption-chicken
  report sum [ local-consumption-chicken ] of kommuner
end

to-report total-consumption-eggs
  report sum [ local-consumption-eggs ] of kommuner
end

to-report total-consumption-milk-cream
  report sum [ local-consumption-milk-cream ] of kommuner
end

to-report total-consumption-yoghurt
  report sum [ local-consumption-yoghurt ] of kommuner
end

to-report total-consumption-butter
  report sum [ local-consumption-butter ] of kommuner
end

to-report total-consumption-cheese
  report sum [ local-consumption-cheese ] of kommuner
end

; Note that we convert the final dairy products into their raw milk equivalent weights in order to make production and consumption comparable.
to-report total-consumption-animal-products
  report (total-consumption-beef + total-consumption-pork + total-consumption-lamb + total-consumption-chicken + total-consumption-eggs + (total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335000) + (total-consumption-butter * 0.2522000) + (total-consumption-cheese * 6.9140000))
end


; NOTES:
; - Prices are set based on per capita production relative to 2020, rather than on ability to meet updated per capita demands so increasing consumption has no impact!
@#$#@#$#@
GRAPHICS-WINDOW
8
10
537
670
-1
-1
1.0
1
10
1
1
1
0
0
0
1
-260
260
-325
325
1
1
1
ticks
30.0

BUTTON
193
672
301
705
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
209
1488
408
1608
Number of dairy cows
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-dairy-cows"

BUTTON
413
672
537
705
Run simulation
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
745
132
945
165
hide-slaughterhouses?
hide-slaughterhouses?
0
1
-1000

CHOOSER
539
191
739
236
population-growth
population-growth
"Low" "Medium" "High"
1

MONITOR
102
672
191
717
Year
year
0
1
11

BUTTON
303
672
411
705
Step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
539
74
739
107
sim-end-yr
sim-end-yr
2013
2050
2050.0
1
1
NIL
HORIZONTAL

PLOT
1721
26
1920
146
Population (m)
Tick
NIL
0.0
37.0
0.0
7.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-population / 1000000"

SWITCH
745
27
945
60
hide-farms?
hide-farms?
1
1
-1000

SLIDER
539
109
739
142
num-farms-to-sim
num-farms-to-sim
1
42437
40382.0
1
1
NIL
HORIZONTAL

PLOT
209
1000
408
1120
Number of sheep
Tick
NIL
0.0
37.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-sheep"

PLOT
209
1122
408
1242
Number of pigs
NIL
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-pigs"

PLOT
209
1244
408
1364
Number of broilers
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-broilers"

PLOT
209
1366
408
1486
Number of laying hens
Ticks
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-laying-hens"

PLOT
8
1000
207
1120
Farms with sheep
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-sheep-farms"

PLOT
8
1122
207
1242
Farms with pigs
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-pig-farms"

PLOT
8
1244
207
1364
Farms with broilers
Ticks
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-broiler-farms"

PLOT
8
1366
207
1486
Farms with laying hens
Ticks
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-laying-hen-farms"

PLOT
8
1488
207
1608
Farms with dairy
Ticks
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-dairy-farms"

PLOT
1721
148
1920
268
Total farms
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farms"

PLOT
8
756
207
876
Farms with livestock
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-livestock-farms"

TEXTBOX
1721
10
1871
28
Overview metrics
11
0.0
1

TEXTBOX
9
738
221
766
Production & consumption metrics
11
0.0
1

PLOT
209
756
408
876
Number of livestock
NIL
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-livestock"

TEXTBOX
745
10
895
28
Visualisation settings
11
0.0
1

TEXTBOX
539
10
689
28
Simulation settings (pt. 1)
11
0.0
1

PLOT
8
1641
207
1761
Farms with grain & oilseed
NIL
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-grain-oilseed-farms"

PLOT
8
1763
207
1883
Farms with other crops
NIL
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-other-field-hort-farms"

PLOT
209
1763
408
1883
Other crops
NIL
hectares
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-ha-other-field-hort"

PLOT
209
1641
408
1761
Grain & oilseeds
NIL
hectares
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-ha-grain-oilseed"

TEXTBOX
8
1624
158
1642
Crop production metrics
11
0.0
1

SWITCH
745
202
945
235
hide-dairies?
hide-dairies?
0
1
-1000

SLIDER
539
273
739
306
slaughter-min-capacity
slaughter-min-capacity
0
100
80.0
5
1
%
HORIZONTAL

PLOT
8
878
207
998
Farms with cattle
NIL
NIL
0.0
37.0
0.0
15318.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-cattle-farms"

PLOT
209
878
408
998
Number of cattle
NIL
NIL
0.0
37.0
0.0
853952.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-cattle"

CHOOSER
539
144
739
189
animal-yield-trajectory
animal-yield-trajectory
"Constant" "Trend"
0

SWITCH
745
97
945
130
hide-fylker?
hide-fylker?
0
1
-1000

SLIDER
950
328
1198
361
consum-growth-beef
consum-growth-beef
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
503
1198
536
consum-growth-milk-cream
consum-growth-milk-cream
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
363
1198
396
consum-growth-pork
consum-growth-pork
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
398
1198
431
consum-growth-lamb
consum-growth-lamb
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
433
1198
466
consum-growth-chicken
consum-growth-chicken
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
468
1198
501
consum-growth-eggs
consum-growth-eggs
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
538
1198
571
consum-growth-yoghurt
consum-growth-yoghurt
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
573
1198
606
consum-growth-butter
consum-growth-butter
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

SLIDER
950
608
1198
641
consum-growth-cheese
consum-growth-cheese
-5
5
0.0
0.05
1
% per yr
HORIZONTAL

TEXTBOX
950
311
1200
339
Per-capita consumption growth post-2020
11
0.0
1

PLOT
1616
878
1815
998
Consumption of beef
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-beef"

PLOT
1616
1244
1815
1364
Consumption of chicken
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-chicken"

PLOT
1616
1122
1815
1242
Consumption of pork
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-pork"

PLOT
1616
1000
1815
1120
Consumption of lamb
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-lamb"

PLOT
1616
1366
1815
1486
Consumption of eggs
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-eggs"

PLOT
1616
1488
1815
1608
Consumption of milk & cream
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-milk-cream"

PLOT
1616
1610
1815
1730
Consumption of yoghurt
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-yoghurt"

PLOT
1616
1732
1815
1852
Consumption of butter
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-butter"

PLOT
1616
1854
1815
1974
Consumption of cheese
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-cheese"

PLOT
1616
756
1815
876
Consumption animal products (with dairy as raw milk equiv.)
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-consumption-animal-products"

PLOT
1415
878
1614
998
Farm-gate price beef
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot current-fgp-cattle-carcass"

CHOOSER
539
413
739
458
price-scenario
price-scenario
"User specified" "Determined by markets"
1

SLIDER
951
27
1199
60
price-growth-beef
price-growth-beef
-5
5
0.0
0.05
1
%
HORIZONTAL

TEXTBOX
952
10
1163
38
Farm-gate price growth post-2020
11
0.0
1

SLIDER
951
62
1199
95
price-growth-pork
price-growth-pork
-5
5
0.0
0.05
1
%
HORIZONTAL

SLIDER
951
97
1199
130
price-growth-lamb
price-growth-lamb
-5
5
0.0
0.05
1
%
HORIZONTAL

SLIDER
951
132
1199
165
price-growth-chicken
price-growth-chicken
-5
5
0.0
0.05
1
%
HORIZONTAL

SLIDER
951
167
1199
200
price-growth-eggs
price-growth-eggs
-5
5
0.0
0.05
1
%
HORIZONTAL

SLIDER
951
202
1199
235
price-growth-raw-milk
price-growth-raw-milk
-5
5
0.0
0.05
1
%
HORIZONTAL

SLIDER
951
237
1199
270
price-growth-wool
price-growth-wool
-5
5
0.0
0.05
1
%
HORIZONTAL

SLIDER
951
272
1199
305
price-growth-crops
price-growth-crops
-5
5
0.0
0.05
1
%
HORIZONTAL

PLOT
1415
1000
1614
1120
Farm-gate price lamb
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot current-fgp-sheep-carcass"

PLOT
1415
1122
1614
1242
Farm-gate price pork
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot current-fgp-pig-carcass"

PLOT
1415
1244
1614
1364
Farm-gate price chicken
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot current-fgp-broiler-carcass"

PLOT
1415
1366
1614
1486
Farm-gate price eggs
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot current-fgp-eggs"

PLOT
1415
1488
1614
1608
Farm-gate price raw milk
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot current-fgp-raw-milk"

SLIDER
539
343
739
376
dairy-min-capacity
dairy-min-capacity
0
100
80.0
5
1
%
HORIZONTAL

PLOT
410
878
609
998
Farm production of beef
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-beef"

PLOT
410
1000
609
1120
Farm production of lamb
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-lamb"

PLOT
410
1122
609
1242
Farm production of pork
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-pork"

PLOT
410
1244
609
1364
Farm production of chicken
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-chicken"

PLOT
410
1366
609
1486
Farm production of eggs
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-eggs"

PLOT
410
1488
609
1608
Farm production of raw milk
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-raw-milk"

PLOT
410
756
609
876
Farm production of animal products
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-animal-products"

PLOT
410
1641
609
1761
Farm production of grain & oilseeds
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-grain-oilseeds"

PLOT
410
1763
609
1883
Farm production of other crops
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-all-other-crops"

SWITCH
745
237
945
270
hide-farm-dairy-links?
hide-farm-dairy-links?
0
1
-1000

SWITCH
745
167
945
200
hide-farm-slaught-links?
hide-farm-slaught-links?
0
1
-1000

SWITCH
745
62
945
95
hide-kommuner?
hide-kommuner?
1
1
-1000

SWITCH
745
272
945
305
hide-checkpoints?
hide-checkpoints?
1
1
-1000

PLOT
1817
878
2016
998
Beef from NO farms
NIL
%
0.0
37.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-farm-production-beef / total-consumption-beef) * 100"

PLOT
1817
1000
2016
1120
Lamb from NO farms
NIL
%
0.0
37.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-farm-production-lamb / total-consumption-lamb) * 100"

PLOT
1817
1122
2016
1242
Pork from NO farms
NIL
%
0.0
37.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-farm-production-pork / total-consumption-pork) * 100"

PLOT
1817
1244
2016
1364
Chicken from NO farms
NIL
%
0.0
37.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-farm-production-chicken / total-consumption-chicken) * 100"

PLOT
1817
1366
2016
1486
Eggs from NO farms
NIL
%
0.0
37.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-farm-production-eggs / total-consumption-eggs) * 100"

PLOT
1817
1488
2016
1608
Dairy from NO farms
NIL
%
0.0
37.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-raw-milk * 0.8894143)  / ((total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335) + (total-consumption-butter * 0.2522) + (total-consumption-cheese * 6.914))) * 100"

PLOT
1817
756
2016
876
Animal products from NO farms
NIL
%
0.0
37.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-farm-production-animal-products / total-consumption-animal-products) * 100"

SLIDER
539
238
739
271
farm-income-viability
farm-income-viability
-100
-5
-15.0
1
1
%
HORIZONTAL

SLIDER
539
308
739
341
max-dist-to-slaughter
max-dist-to-slaughter
230
1000
230.0
1
1
km
HORIZONTAL

SLIDER
539
378
739
411
max-dist-to-dairy
max-dist-to-dairy
140
1000
140.0
1
1
km
HORIZONTAL

PLOT
1721
270
1920
390
Total slaughterhouses
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count slaughterhouses with [ active? = TRUE ]"

PLOT
1721
392
1920
512
Total dairies
Tick
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count dairies with [ active? = TRUE ]"

SLIDER
539
647
739
680
cm-max-share
cm-max-share
0
100
53.9
0.1
1
%
HORIZONTAL

SLIDER
539
507
739
540
price-response-ratio
price-response-ratio
0
5
1.0
0.25
1
%
HORIZONTAL

CHOOSER
539
460
739
505
price-baseline-year
price-baseline-year
2020
0

CHOOSER
539
27
739
72
param-scenario
param-scenario
"Default" "Custom"
0

CHOOSER
539
682
739
727
cm-scenario
cm-scenario
"Scenario 6" "Scenario 7" "Scenario 8"
2

SLIDER
539
577
739
610
cm-init-yr
cm-init-yr
2021
2049
2024.0
1
1
NIL
HORIZONTAL

SWITCH
539
542
739
575
sim-cm?
sim-cm?
0
1
-1000

TEXTBOX
485
439
511
457
Beef
11
0.0
1

TEXTBOX
358
420
488
438
Cultured meat factories
11
0.0
1

PLOT
2018
878
2217
998
Beef from CM factories
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-factory-production-beef / total-consumption-beef) * 100"

PLOT
2018
1000
2217
1120
Lamb from CM factories
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-factory-production-lamb / total-consumption-lamb) * 100"

PLOT
2018
1122
2217
1242
Pork from CM factories
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-factory-production-pork / total-consumption-pork) * 100"

PLOT
2018
1244
2217
1364
Chicken from CM factories
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-factory-production-chicken / total-consumption-chicken) * 100"

PLOT
611
878
810
998
Factory production of beef
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count cm-factories with [ meat-type = \"Beef\" ] * cm-factory-capacity)"

PLOT
611
1000
810
1120
Factory production of lamb
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count cm-factories with [ meat-type = \"Lamb\" ] * cm-factory-capacity"

PLOT
611
1122
810
1242
Factory production of pork
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count cm-factories with [ meat-type = \"Pork\" ] * cm-factory-capacity)"

PLOT
611
1244
810
1364
Factory production of chicken
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count cm-factories with [ meat-type = \"Chicken\" ] * cm-factory-capacity)"

SLIDER
539
612
739
645
cm-factory-capacity
cm-factory-capacity
0
10000
5000.0
1000
1
tonnes
HORIZONTAL

TEXTBOX
485
468
515
486
Lamb
11
0.0
1

TEXTBOX
485
454
517
472
Pork
11
0.0
1

TEXTBOX
485
484
529
502
Chicken
11
0.0
1

PLOT
2219
878
2418
998
Beef self-sufficiency
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-beef + total-factory-production-beef) / total-consumption-beef) * 100"

PLOT
2219
1000
2418
1120
Lamb self-sufficiency
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-lamb + total-factory-production-lamb) / total-consumption-lamb) * 100"

PLOT
2219
1122
2418
1242
Pork self-sufficiency
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-pork + total-factory-production-pork) / total-consumption-pork) * 100"

PLOT
2219
1244
2418
1364
Chicken self-sufficiency
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-chicken + total-factory-production-chicken) / total-consumption-chicken) * 100"

CHOOSER
744
682
944
727
pf-scenario
pf-scenario
"Scenario 1" "Scenario 2"
1

SLIDER
744
542
944
575
pf-init-yr
pf-init-yr
2021
2049
2024.0
1
1
NIL
HORIZONTAL

SLIDER
744
577
944
610
pf-factory-dairy-capacity
pf-factory-dairy-capacity
0
425000
170000.0
10000
1
tonnes
HORIZONTAL

SWITCH
744
507
944
540
sim-pf?
sim-pf?
0
1
-1000

SLIDER
744
647
944
680
pf-max-share
pf-max-share
0
100
53.9
0.1
1
%
HORIZONTAL

SLIDER
744
612
944
645
pf-factory-egg-capacity
pf-factory-egg-capacity
0
10000
5000.0
1000
1
tonnes
HORIZONTAL

PLOT
611
1366
810
1486
Factory production of eggs
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count pf-factories with [ product-type = \"Eggs\" ] * pf-factory-egg-capacity)"

TEXTBOX
485
632
515
650
Eggs
11
0.0
1

PLOT
2018
1366
2217
1486
Egg from PF factories
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-factory-production-eggs / total-consumption-eggs) * 100"

PLOT
2219
1366
2418
1486
Egg self-sufficiency
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-eggs + total-factory-production-eggs) / total-consumption-eggs) * 100"

PLOT
1922
270
2121
390
Total CM factories
NIL
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count cm-factories"

PLOT
1922
392
2121
512
Total PF milk factories
NIL
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count pf-factories with [ product-type = \"Dairy\" ]"

PLOT
1922
514
2121
634
Total PF egg factories
NIL
NIL
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count pf-factories with [ product-type = \"Eggs\" ]"

TEXTBOX
745
384
895
402
Simulation settings (pt. 2)
11
0.0
1

TEXTBOX
484
616
511
634
Milk
11
0.0
1

TEXTBOX
334
600
510
628
Precision fermented factories
11
0.0
1

PLOT
611
1488
810
1608
Factory production of raw milk
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count pf-factories with [ product-type = \"Dairy\" ] * pf-factory-dairy-capacity)"

PLOT
2018
1488
2217
1608
Dairy from PF factories
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-factory-production-raw-milk  / ((total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335) + (total-consumption-butter * 0.2522) + (total-consumption-cheese * 6.914))) * 100"

PLOT
2219
1488
2418
1608
Dairy self-sufficiency
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (((total-farm-production-raw-milk * 0.8894143) + total-factory-production-raw-milk)  / ((total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335) + (total-consumption-butter * 0.2522) + (total-consumption-cheese * 6.914))) * 100"

PLOT
611
756
810
876
Factory production of animal products
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count cm-factories with [ meat-type = \"Beef\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Lamb\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Pork\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Chicken\" ] * cm-factory-capacity) + (count pf-factories with [ product-type = \"Eggs\" ] * pf-factory-egg-capacity) + (count pf-factories with [ product-type = \"Dairy\" ] * pf-factory-dairy-capacity)"

PLOT
2018
756
2217
876
Animal products from factories
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-factory-production-beef + total-factory-production-lamb + total-factory-production-pork + total-factory-production-chicken + total-factory-production-eggs + total-factory-production-raw-milk) / (total-consumption-beef + total-consumption-lamb + total-consumption-pork + total-consumption-chicken + total-consumption-eggs + (total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335) + (total-consumption-butter * 0.2522) + (total-consumption-cheese * 6.914))) * 100"

PLOT
2219
756
2418
876
Animal product self-sufficiency
NIL
%
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-beef + total-factory-production-beef + total-farm-production-lamb + total-factory-production-lamb + total-farm-production-pork + total-factory-production-pork + total-farm-production-chicken + total-factory-production-chicken + total-farm-production-eggs + total-factory-production-eggs + (total-farm-production-raw-milk * 0.8894143) + total-factory-production-raw-milk) / (total-consumption-beef + total-consumption-lamb + total-consumption-pork + total-consumption-chicken + total-consumption-eggs + (total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335) + (total-consumption-butter * 0.2522) + (total-consumption-cheese * 6.914))) * 100"

SLIDER
744
402
944
435
efficiency-gain-multiplier
efficiency-gain-multiplier
0.5
1
0.95
0.01
1
NIL
HORIZONTAL

SLIDER
744
437
944
470
efficiency-step-int-nonmilk
efficiency-step-int-nonmilk
5000
75000
20000.0
5000
1
tonnes
HORIZONTAL

SLIDER
744
472
944
505
efficiency-step-int-milk
efficiency-step-int-milk
170000
850000
340000.0
170000
1
tonnes
HORIZONTAL

PLOT
1214
878
1413
998
Cost of cultured beef
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot cm-cost-beef * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)"

PLOT
1214
1000
1413
1120
Cost of cultured lamb
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot cm-cost-lamb * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)"

PLOT
1214
1122
1413
1242
Cost of cultured pork
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot cm-cost-pork * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)"

PLOT
1214
1244
1413
1364
Cost of cultured chicken
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot cm-cost-chicken * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)"

PLOT
1214
1366
1413
1486
Cost of PF eggs
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot pf-cost-eggs * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)"

PLOT
1214
1488
1413
1608
Cost of PF raw milk
NIL
NOK/t
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot pf-cost-dairy * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)"

PLOT
812
878
1011
998
Total production of beef
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-beef + (count cm-factories with [ meat-type = \"Beef\" ] * cm-factory-capacity)"

PLOT
812
1000
1011
1120
Total production of lamb
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-lamb + (count cm-factories with [ meat-type = \"Lamb\" ] * cm-factory-capacity)"

PLOT
812
1122
1011
1242
Total production of pork
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-pork + (count cm-factories with [ meat-type = \"Pork\" ] * cm-factory-capacity)"

PLOT
812
1244
1011
1364
Total production of chicken
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-chicken + (count cm-factories with [ meat-type = \"Chicken\" ] * cm-factory-capacity)"

PLOT
812
1366
1011
1486
Total production of eggs
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-eggs + (count pf-factories with [ product-type = \"Eggs\" ] * pf-factory-egg-capacity)"

PLOT
812
1488
1011
1608
Total production of raw milk
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-raw-milk + (count pf-factories with [ product-type = \"Dairy\" ] * pf-factory-dairy-capacity)"

PLOT
812
756
1011
876
Total production of animal products
NIL
tonnes
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-production-animal-products + (count cm-factories with [ meat-type = \"Beef\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Lamb\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Pork\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Chicken\" ] * cm-factory-capacity) + (count pf-factories with [ product-type = \"Eggs\" ] * pf-factory-egg-capacity) + (count pf-factories with [ product-type = \"Dairy\" ] * pf-factory-dairy-capacity)"

PLOT
1013
756
1212
876
Per capita production of animal products
NIL
kg
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-animal-products + (count cm-factories with [ meat-type = \"Beef\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Lamb\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Pork\" ] * cm-factory-capacity) + (count cm-factories with [ meat-type = \"Chicken\" ] * cm-factory-capacity) + (count pf-factories with [ product-type = \"Eggs\" ] * pf-factory-egg-capacity) + (count pf-factories with [ product-type = \"Dairy\" ] * pf-factory-dairy-capacity)) / total-population) * 1000"

PLOT
1013
878
1212
998
Per capita production of beef
NIL
kg
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-beef + (count cm-factories with [ meat-type = \"Beef\" ] * cm-factory-capacity)) / total-population) * 1000"

PLOT
1013
1000
1212
1120
Per capita production of lamb
NIL
kg
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-lamb + (count cm-factories with [ meat-type = \"Lamb\" ] * cm-factory-capacity)) / total-population) * 1000"

PLOT
1013
1122
1212
1242
Per capita production of pork
NIL
kg
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-pork + (count cm-factories with [ meat-type = \"Pork\" ] * cm-factory-capacity)) / total-population) * 1000"

PLOT
1013
1244
1212
1364
Per capita production of chicken
NIL
kg
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-chicken + (count cm-factories with [ meat-type = \"Chicken\" ] * cm-factory-capacity)) / total-population) * 1000"

PLOT
1013
1366
1212
1486
Per capita production of eggs
NIL
kg
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-eggs + (count pf-factories with [ product-type = \"Eggs\" ] * pf-factory-egg-capacity)) / total-population) * 1000"

PLOT
1013
1488
1212
1608
Per capita production of raw milk
NIL
kg
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-raw-milk + (count pf-factories with [ product-type = \"Dairy\" ] * pf-factory-dairy-capacity)) / total-population) * 1000"

TEXTBOX
1205
432
1568
474
Emissions by activity inc. energy & LULUCF (agricultural emission coefficients based on Table 12 Mittenzwei and Prestvik (2022))
11
0.0
1

MONITOR
1406
510
1560
555
kt CO2-eq. livestock
precision total-emissions-livestock 2
17
1
11

MONITOR
1406
463
1560
508
kt CO2-eq. crops
precision total-emissions-crops 2
17
1
11

MONITOR
1406
699
1560
744
kt CO2-eq. total
precision total-emissions 2
17
1
11

MONITOR
1562
463
1716
508
% emissions crops
precision emissions-share-crops 2
17
1
11

MONITOR
1562
510
1716
555
% emissions livestock
precision emissions-share-livestock 2
17
1
11

CHOOSER
8
672
100
717
start-yr
start-yr
2013 2020
1

PLOT
1205
463
1404
744
Emissions by activity
NIL
kt CO2-eq.
0.0
37.0
0.0
10.0
true
false
"" ""
PENS
"Barley" 1.0 0 -2166824 true "" "plot total-emissions-barley"
"Oats" 1.0 0 -2166824 true "" "plot total-emissions-oats"
"Wheat" 1.0 0 -2166824 true "" "plot total-emissions-wheat"
"Rye & triticale" 1.0 0 -2166824 true "" "plot total-emissions-rye-triticale"
"Oilseeds" 1.0 0 -2166824 true "" "plot total-emissions-oilseeds"
"Potatoes" 1.0 0 -2166824 true "" "plot total-emissions-potatoes"
"Vegetables" 1.0 0 -2166824 true "" "plot total-emissions-vegetables"
"Green fodder & silage" 1.0 0 -2166824 true "" "plot total-emissions-fodder-silage"
"Other crops" 1.0 0 -2166824 true "" "plot total-emissions-other-crops"
"Orchards" 1.0 0 -2166824 true "" "plot total-emissions-orchards"
"Berries" 1.0 0 -2166824 true "" "plot total-emissions-berries"
"All crops" 1.0 0 -13210332 true "" "plot total-emissions-crops"
"Dairy cows" 1.0 0 -1069655 true "" "plot total-emissions-dairy-cows"
"Beef cows" 1.0 0 -1069655 true "" "plot total-emissions-beef-cows"
"Other cattle" 1.0 0 -1069655 true "" "plot total-emissions-other-cattle"
"Sheep" 1.0 0 -1069655 true "" "plot total-emissions-sheep"
"Pigs" 1.0 0 -1069655 true "" "plot total-emissions-pigs"
"Broilers" 1.0 0 -1069655 true "" "plot total-emissions-broilers"
"Laying hens" 1.0 0 -1069655 true "" "plot total-emissions-laying-hens"
"All livestock" 1.0 0 -8053223 true "" "plot total-emissions-livestock"
"All agriculture" 1.0 0 -11053225 true "" "plot total-emissions-agriculture"
"CM" 1.0 0 -5516827 true "" "plot total-emissions-cm"
"PF milk" 1.0 0 -5516827 true "" "plot total-emissions-pf-dairy"
"PF egg" 1.0 0 -5516827 true "" "plot total-emissions-pf-egg"
"All CF" 1.0 0 -14454117 true "" "plot total-emissions-cf"
"Total emissions" 1.0 0 -16777216 true "" "plot total-emissions"

CHOOSER
1205
88
1404
133
emissions-pf-dairy
emissions-pf-dairy
0.091327 0.4044
1

CHOOSER
1205
135
1404
180
emissions-pf-egg
emissions-pf-egg
0.3523 1.56
1

CHOOSER
1205
41
1404
86
emissions-cm-meat
emissions-cm-meat
2.9 4.1
0

TEXTBOX
1408
40
1714
180
Emission coefficients for conventionally produced products in Norway are as follows (Source: Mittenzwei and Prestvik, 2022):\n\n• Chicken: 0.69\n• Pork: 2.022\n• Lamb: 37.207\n• Beef: 33.392-54.346\n• Raw milk: 1.139\n• Eggs: 0.777
11
0.0
1

TEXTBOX
1205
10
1407
38
Emission coefficients for CF products (kg CO2-eq. per kg)
11
0.0
1

TEXTBOX
1204
188
1462
230
Average emissions from domestic production (kg CO2-eq. per kg)
11
0.0
1

MONITOR
1406
651
1560
696
kt CO2-eq. PF egg
precision total-emissions-pf-egg 2
17
1
11

MONITOR
1406
604
1560
649
kt CO2-eq. PF dairy
precision total-emissions-pf-dairy 2
17
1
11

MONITOR
1406
557
1560
602
kt CO2-eq. CM meat
precision total-emissions-cm 2
17
1
11

MONITOR
1562
651
1716
696
% emissions PF egg
precision emissions-share-pf-egg 2
17
1
11

MONITOR
1562
604
1716
649
% emissions PF dairy
precision emissions-share-pf-dairy 2
17
1
11

MONITOR
1562
557
1716
602
% emissions CM meat
precision emissions-share-cm 2
17
1
11

PLOT
1204
218
1456
428
Average emissions per kg
NIL
kg CO2-eq. per kg
0.0
37.0
0.0
1.0
true
true
"" ""
PENS
"Eggs" 1.0 0 -4079321 true "" "plot av-emissions-kg-egg"
"Dairy" 1.0 0 -12345184 true "" "plot av-emissions-kg-dairy"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

biosynth-factory
false
0
Polygon -7500403 true true 30 240 270 240 270 90 150 30 30 90 30 240

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
