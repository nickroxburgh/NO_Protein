extensions [ gis csv ]

globals [ starting-seed year kommuner-list fylker-list farms-list slaughterhouse-list dairy-list checkpoint-list pp-barley pp-oats pp-wheat pp-rye-triticale pp-oilseeds pp-potatoes pp-vegetables pp-fodder-silage pp-other-crops pp-pome-stone-fruit pp-berries pp-other-cattle-meat pp-beef-cow-meat pp-dairy-cow-meat pp-raw-milk pp-pig-meat pp-sheep-meat pp-broiler-meat pp-wool pp-eggs dist-coeff total-imports-beef total-imports-pork total-imports-lamb total-imports-chicken total-imports-eggs total-imports-milk-cream total-imports-yoghurt total-imports-butter total-imports-cheese total-exports-beef total-exports-pork total-exports-lamb total-exports-chicken total-exports-eggs total-exports-milk-cream total-exports-yoghurt total-exports-butter total-exports-cheese production-per-capita-beef production-per-capita-pork production-per-capita-lamb production-per-capita-chicken production-per-capita-eggs production-per-capita-wool production-per-capita-rawmilk cm-cost-beef cm-cost-pork cm-cost-lamb cm-cost-chicken pf-cost-dairy pf-cost-eggs aggregate-production-cm-meat aggregate-production-pf-dairy aggregate-production-pf-eggs emissions-ha-barley emissions-ha-oats emissions-ha-wheat emissions-ha-rye-triticale emissions-ha-oilseeds emissions-ha-potatoes emissions-ha-vegetables emissions-ha-fodder-silage emissions-ha-other-crops emissions-ha-orchards emissions-ha-berries emissions-head-dairy-cows emissions-head-beef-cows emissions-head-other-cattle emissions-head-sheep emissions-head-pigs emissions-head-broilers emissions-head-laying-hens init-num-specialist-cattle-farms init-num-specialist-sheep-farms init-num-specialist-pig-farms init-num-specialist-broiler-farms init-num-specialist-laying-hen-farms init-num-specialist-fodder-silage-farms init-num-specialist-arable-horticulture-farms init-num-combined-cattle-grain-farms init-num-combined-cattle-sheep-farms init-num-other-mixed-farms init-num-no-activity-farms ]

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
fylker-own [ fylke-id yield-barley yield-oats yield-wheat yield-rye-triticale yield-oilseeds yield-potatoes yield-vegetables yield-fodder-silage yield-other-crops yield-orchards yield-berries yield-other-cattle-meat yield-beef-cow-meat yield-dairy-cow-meat yield-raw-milk yield-pig-meat yield-sheep-meat yield-broiler-meat yield-wool yield-eggs ]
farms-own [ farm-id farm-type active? kommune-id fylke-id partner-dairy dairy-distance-km partner-slaughterhouse slaughterhouse-distance-km ha-barley ha-oats ha-wheat ha-rye-triticale ha-oilseeds ha-potatoes ha-vegetables ha-fodder-silage ha-other-crops ha-orchards ha-berries num-dairy-cows num-beef-cows num-other-cattle num-sheep num-pigs num-broilers num-laying-hens subsidy-nonlivestock subsidy-livestock subsidy-milk benchmark-income annual-income recent-income current-income annual-emissions current-emissions annual-carbon-tax-liability current-carbon-tax-liability ]
slaughterhouses-own [ slaughterhouse-name min-viable-meat supply-beef supply-pork supply-lamb supply-chicken supply-eggs processed-beef processed-pork processed-lamb processed-chicken processed-eggs wholesale-stock-beef wholesale-stock-pork wholesale-stock-lamb wholesale-stock-chicken wholesale-stock-eggs active? ]
dairies-own[ dairy-name min-viable-rawmilk supply-rawmilk processed-rawmilk wholesale-stock-milk-cream wholesale-stock-yoghurt wholesale-stock-butter wholesale-stock-cheese active? ]
checkpoints-own [ checkpoint-id checkpoint-name checkpoint-type destination imports-beef imports-pork imports-lamb imports-chicken imports-milk-cream imports-yoghurt imports-butter imports-cheese imports-eggs exports-beef exports-pork exports-lamb exports-chicken exports-milk-cream exports-yoghurt exports-butter exports-cheese exports-eggs ]
cm-factories-own [ product-type production-quota wholesale-stock processed-meat factory-emissions ]
pf-factories-own [ product-type production-quota wholesale-stock-milk-cream wholesale-stock-yoghurt wholesale-stock-butter wholesale-stock-cheese wholesale-stock-eggs processed-rawmilk processed-eggs factory-emissions ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP                                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  ca
  clear-all
  file-close-all
  set starting-seed new-seed
  random-seed starting-seed
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
  setup-producer-price
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
      set yield-other-cattle-meat []
      set yield-beef-cow-meat []
      set yield-dairy-cow-meat []
      set yield-raw-milk []
      set yield-pig-meat []
      set yield-sheep-meat []
      set yield-broiler-meat []
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
  if start-yr = 2013 [ set num-farms-to-sim 42437 ]
  if start-yr = 2020 [ set num-farms-to-sim 40382 ]
  file-close-all
  ; One at a time, add ports as agents.
  foreach n-of num-farms-to-sim gis:feature-list-of farms-list [ current-farm ->
    let centroid gis:location-of gis:centroid-of current-farm
    create-farms 1 [
      ; Assign each farm their ID, kommune ID, and details of their activities, plus initialise lists
      ; that will be used to determine the benchmark income they aspire to and their annual income.
      set farm-id gis:property-value current-farm "FarmerID"
      set farm-type gis:property-value current-farm "FarmType"
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
      set current-income 0
      set annual-emissions []
      set current-emissions 0
      set annual-carbon-tax-liability []
      set current-carbon-tax-liability 0
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
  ; Log the initial number of farms of each type to the relevant global variables.
  set init-num-specialist-cattle-farms specialist-cattle-farms
  set init-num-specialist-sheep-farms specialist-sheep-farms
  set init-num-specialist-pig-farms specialist-pig-farms
  set init-num-specialist-broiler-farms specialist-broiler-farms
  set init-num-specialist-laying-hen-farms specialist-laying-hen-farms
  set init-num-specialist-fodder-silage-farms specialist-fodder-silage-farms
  set init-num-specialist-arable-horticulture-farms specialist-arable-horticulture-farms
  set init-num-combined-cattle-grain-farms combined-cattle-grain-farms
  set init-num-combined-cattle-sheep-farms combined-cattle-sheep-farms
  set init-num-other-mixed-farms other-mixed-farms
  set init-num-no-activity-farms no-activity-farms
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
      ; capacity slider. The min-viable-meat threshold is the point at which the slaughterhouse is
      ; no longer considered viable. It is the total meat weight processed in 2020, multiplied by
      ; a slaughter-min-capacity slider value. The wholesale-meat lists are records of the amount of
      ; meat the slaughterhouse have processed in each given year.
      set slaughterhouse-name gis:property-value current-slaughterhouse "Slaughterhouse"
      set min-viable-meat 0
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
        ; Extract the mean yield and SD values for the current crop and fylker.
        let yield-mean item 3 row
        let yield-sd item 4 row
        ; There SD of the green fodder and silage is very high for some fylker. This variability leads to
        ; a lot of farms ceasing operations at some point in the model, yet there the degree of variability
        ; in the data seems suspect. As with the berries, oilseeds, orchards, vegetables, and other crop
        ; categories (which lack sufficient data to assess variability with confidence), we have therefore
        ; decided to not simulate temporal variability in green fodder and silage yields, only spatial
        ; variability. Here we reset the SD for these cases to zero.
        if item 2 row = "Green fodder & silage" [ set yield-sd 0 ]
        ; Ask this fylke to generate a yield value for the current crop based on the mean and SD.
        let current-yield precision (random-normal yield-mean yield-sd) 2
        ; If the generated value is more than two standard deviations above or below the mean, we
        ; set the number to the two standard deviations threshold. This prevents negative yield
        ; values which are not possible in the real world and puts bounds on the possible extreme
        ; values. This is reasonable to do in a Western agricultural context where there typically
        ; are upper and lower bounds to viable yields.
        if current-yield < (yield-mean - (yield-sd * 2)) [
          set current-yield yield-mean - (yield-sd * 2)
        ]
        if current-yield > (yield-mean + (yield-sd * 2)) [
          set current-yield yield-mean + (yield-sd * 2)
        ]
        ; Add the generated value to the end of the nested list for the crop of interest.
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
        if item 3 row = "Other cattle meat" [ set yield-other-cattle-meat lput list (current-year) (current-yield) yield-other-cattle-meat ]
        if item 3 row = "Beef cow meat" [ set yield-beef-cow-meat lput list (current-year) (current-yield) yield-beef-cow-meat ]
        if item 3 row = "Dairy cow meat" [ set yield-dairy-cow-meat lput list (current-year) (current-yield) yield-dairy-cow-meat ]
        if item 3 row = "Raw milk" [ set yield-raw-milk lput list (current-year) (current-yield) yield-raw-milk ]
        if item 3 row = "Pig meat" [ set yield-pig-meat lput list (current-year) (current-yield) yield-pig-meat ]
        if item 3 row = "Sheep meat" [ set yield-sheep-meat lput list (current-year) (current-yield) yield-sheep-meat ]
        if item 3 row = "Broiler meat" [ set yield-broiler-meat lput list (current-year) (current-yield) yield-broiler-meat ]
        if item 3 row = "Wool" [ set yield-wool lput list (current-year) (current-yield) yield-wool ]
        if item 3 row = "Eggs" [ set yield-eggs lput list (current-year) (current-yield) yield-eggs ]
      ]
    ]
  ]
  file-close
end

to setup-producer-price
  ; Initialise producer price lists.
  set pp-barley []
  set pp-oats []
  set pp-wheat []
  set pp-rye-triticale []
  set pp-oilseeds []
  set pp-potatoes []
  set pp-vegetables []
  set pp-fodder-silage []
  set pp-other-crops []
  set pp-pome-stone-fruit []
  set pp-berries []
  set pp-other-cattle-meat []
  set pp-beef-cow-meat []
  set pp-dairy-cow-meat []
  set pp-raw-milk []
  set pp-pig-meat []
  set pp-sheep-meat []
  set pp-broiler-meat []
  set pp-wool []
  set pp-eggs []
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
      if item 1 row = "Barley" [ set pp-barley lput list (current-year) (current-price) pp-barley ]
      if item 1 row = "Oats" [ set pp-oats lput list (current-year) (current-price) pp-oats ]
      if item 1 row = "Wheat" [ set pp-wheat lput list (current-year) (current-price) pp-wheat ]
      if item 1 row = "Rye & triticale" [ set pp-rye-triticale lput list (current-year) (current-price) pp-rye-triticale ]
      if item 1 row = "Oilseeds" [ set pp-oilseeds lput list (current-year) (current-price) pp-oilseeds ]
      if item 1 row = "Potatoes" [ set pp-potatoes lput list (current-year) (current-price) pp-potatoes ]
      if item 1 row = "Vegetables" [ set pp-vegetables lput list (current-year) (current-price) pp-vegetables ]
      if item 1 row = "Green fodder & silage" [ set pp-fodder-silage lput list (current-year) (current-price) pp-fodder-silage ]
      if item 1 row = "Other crops" [ set pp-other-crops lput list (current-year) (current-price) pp-other-crops ]
      if item 1 row = "Pome & stone fruits" [ set pp-pome-stone-fruit lput list (current-year) (current-price) pp-pome-stone-fruit ]
      if item 1 row = "Berries" [ set pp-berries lput list (current-year) (current-price) pp-berries ]
      ; The post-2020 livestock product prices are set seperately so we cease reading them in after 2020.
      if current-year <= 2020 [
        if item 1 row = "Other cattle meat" [ set pp-other-cattle-meat lput list (current-year) (current-price) pp-other-cattle-meat ]
        if item 1 row = "Beef cow meat" [ set pp-beef-cow-meat lput list (current-year) (current-price) pp-beef-cow-meat ]
        if item 1 row = "Dairy cow meat" [ set pp-dairy-cow-meat lput list (current-year) (current-price) pp-dairy-cow-meat ]
        if item 1 row = "Raw milk" [ set pp-raw-milk lput list (current-year) (current-price) pp-raw-milk ]
        if item 1 row = "Pig meat" [ set pp-pig-meat lput list (current-year) (current-price) pp-pig-meat ]
        if item 1 row = "Sheep meat" [ set pp-sheep-meat lput list (current-year) (current-price) pp-sheep-meat ]
        if item 1 row = "Broiler meat" [ set pp-broiler-meat lput list (current-year) (current-price) pp-broiler-meat ]
        if item 1 row = "Wool" [ set pp-wool lput list (current-year) (current-price) pp-wool ]
        if item 1 row = "Eggs" [ set pp-eggs lput list (current-year) (current-price) pp-eggs ]
      ]
    ]
  ]
  file-close
  ; If the price scenario selected calls for prices to be set by users...
  if price-scenario = "User specified" [
    ; The empirical data used to inform the producer price scenarios only takes us to 2020 so we need
    ; to determine values for subsequent years. We do this by assuming constant year-on-year growth
    ; with the rate of growth determined by the `price-growth-product` sliders. It can be between
    ; -5% and 5%.
    let current-year 2021
    loop [
      if current-year > sim-end-yr [ stop ]
      ; Determine the previous year's beef price (which is the same for all cattle types), then calculate
      ; what the current price should be given the slider specified growth, then add it to the pp lists
      ; of the various cattle types.
      let previous-pp filter [ i -> (current-year - 1) = item 0 i ] pp-other-cattle-meat
      set previous-pp item 1 item 0 previous-pp
      let current-pp (previous-pp + (previous-pp * (price-growth-beef / 100)))
      set pp-other-cattle-meat lput list (current-year) (current-pp) pp-other-cattle-meat
      set pp-dairy-cow-meat lput list (current-year) (current-pp) pp-dairy-cow-meat
      set pp-beef-cow-meat lput list (current-year) (current-pp) pp-beef-cow-meat
      ; Repeat for pork
      set previous-pp filter [ i -> (current-year - 1) = item 0 i ] pp-pig-meat
      set previous-pp item 1 item 0 previous-pp
      set current-pp (previous-pp + (previous-pp * (price-growth-pork / 100)))
      set pp-pig-meat lput list (current-year) (current-pp) pp-pig-meat
      ; Repeat for lamb
      set previous-pp filter [ i -> (current-year - 1) = item 0 i ] pp-sheep-meat
      set previous-pp item 1 item 0 previous-pp
      set current-pp (previous-pp + (previous-pp * (price-growth-lamb / 100)))
      set pp-sheep-meat lput list (current-year) (current-pp) pp-sheep-meat
      ; Repeat for chicken
      set previous-pp filter [ i -> (current-year - 1) = item 0 i ] pp-broiler-meat
      set previous-pp item 1 item 0 previous-pp
      set current-pp (previous-pp + (previous-pp * (price-growth-chicken / 100)))
      set pp-broiler-meat lput list (current-year) (current-pp) pp-broiler-meat
      ; Repeat for eggs
      set previous-pp filter [ i -> (current-year - 1) = item 0 i ] pp-eggs
      set previous-pp item 1 item 0 previous-pp
      set current-pp (previous-pp + (previous-pp * (price-growth-eggs / 100)))
      set pp-eggs lput list (current-year) (current-pp) pp-eggs
      ; Repeat for raw milk
      set previous-pp filter [ i -> (current-year - 1) = item 0 i ] pp-raw-milk
      set previous-pp item 1 item 0 previous-pp
      set current-pp (previous-pp + (previous-pp * (price-growth-raw-milk / 100)))
      set pp-raw-milk lput list (current-year) (current-pp) pp-raw-milk
      ; Repeat for wool
      set previous-pp filter [ i -> (current-year - 1) = item 0 i ] pp-wool
      set previous-pp item 1 item 0 previous-pp
      set current-pp (previous-pp + (previous-pp * (price-growth-wool / 100)))
      set pp-wool lput list (current-year) (current-pp) pp-wool
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
  ; Initialise the slaughterhouse min and max meat processing capacity by calculating initial
  ; meat production of partner farms and applying the buffers specified by slaughter-min-capacity and
  ; slaughter-max-capacity.
  ask slaughterhouses [
    let meat-supply-initial 0
    ask my-farm-slaughterhouse-links [ ask other-end [ set meat-supply-initial meat-supply-initial + (farm-production-beef + farm-production-lamb + farm-production-pork + farm-production-chicken) ] ]
    set min-viable-meat (meat-supply-initial / 100) * slaughter-min-capacity
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
  ; Emissions values are given in terms of kg CO2-equiv. per ha or head.
  file-close-all
  file-open "Data/NO_Emissions/ProcessedData/NO_NetLogoEmissions.csv"
  while [ not file-at-end? ] [
    ; Use the CSV extension to grab a line at a time and extract the values.
    let row csv:from-row file-read-line
    let current-activity item 0 row
    let current-value 0
    if emissions-tax-coverage = "Agriculture" [ set current-value item 1 row ]
    if emissions-tax-coverage = "Agriculture & energy" [ set current-value item 2 row ]
    if emissions-tax-coverage = "Agriculture, energy & LULUCF" [ set current-value item 3 row ]
    if current-activity = "Barley" [ set emissions-ha-barley current-value ]
    if current-activity = "Oats" [ set emissions-ha-oats current-value ]
    if current-activity = "Wheat" [ set emissions-ha-wheat current-value ]
    if current-activity = "Rye & triticale" [ set emissions-ha-rye-triticale current-value ]
    if current-activity = "Oilseeds" [ set emissions-ha-oilseeds current-value ]
    if current-activity = "Potatoes" [ set emissions-ha-potatoes current-value ]
    if current-activity = "Vegetables" [ set emissions-ha-vegetables current-value ]
    if current-activity = "Green fodder & silage" [ set emissions-ha-fodder-silage current-value ]
    if current-activity = "Other crops" [ set emissions-ha-other-crops current-value ]
    if current-activity = "Orchards" [ set emissions-ha-orchards current-value ]
    if current-activity = "Berries" [ set emissions-ha-berries current-value ]
    if current-activity = "Dairy cows" [ set emissions-head-dairy-cows current-value ]
    if current-activity = "Beef cows" [ set emissions-head-beef-cows current-value ]
    if current-activity = "Other cattle" [ set emissions-head-other-cattle current-value ]
    if current-activity = "Sheep" [ set emissions-head-sheep current-value ]
    if current-activity = "Pigs" [ set emissions-head-pigs current-value ]
    if current-activity = "Broilers" [ set emissions-head-broilers current-value ]
    if current-activity = "Laying hens" [ set emissions-head-laying-hens current-value ]
  ]
  file-close
end

to set-params-to-default
  set start-yr 2020
  set sim-end-yr 2050
  set num-farms-to-sim 40382
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
  set cease-farming-prob 0.25
  set sim-cm? TRUE
  ; Note that with the cm-init-yr and the pf-init-yr, it will be the next year before production comes
  ; online. This reflects the lag between commisioning and openning such facilities.
  set cm-init-yr 2024
  set cm-factory-capacity 5000
  set cm-scenario "Scenario 7"
  set cm-max-share 53.9
  set sim-pf? TRUE
  set pf-init-yr 2024
  set pf-factory-dairy-capacity 170000
  set pf-factory-egg-capacity 5000
  set pf-scenario "Scenario 2"
  set pf-max-share 53.9
  set efficiency-gain-multiplier 0.95
  set efficiency-step-int-nonmilk 20000
  set efficiency-step-int-milk 680000
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
  set emissions-tax-coverage "Agriculture & energy"
  set emissions-cm-meat 4.1
  set emissions-pf-dairy 0.4044
  set emissions-pf-egg 1.56
  set carbon-tax-per-tonne 1500
  set carbon-tax-start-yr 2025
  set cf-required-profit-margin 10
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
    ; Farms calculate their emissions for the year (values are given in kg CO2-equiv.).
    set current-emissions round ((ha-barley * emissions-ha-barley) + (ha-oats * emissions-ha-oats) + (ha-wheat * emissions-ha-wheat) + (ha-rye-triticale * emissions-ha-rye-triticale) + (ha-oilseeds * emissions-ha-oilseeds) + (ha-potatoes * emissions-ha-potatoes) + (ha-vegetables * emissions-ha-vegetables) + (ha-fodder-silage * emissions-ha-fodder-silage) + (ha-other-crops * emissions-ha-other-crops) + (ha-orchards * emissions-ha-orchards) + (ha-berries * emissions-ha-berries) + (num-dairy-cows * emissions-head-dairy-cows) + (num-beef-cows * emissions-head-beef-cows) + (num-other-cattle * emissions-head-other-cattle) + (num-sheep * emissions-head-sheep) + (num-pigs * emissions-head-pigs) + (num-broilers * emissions-head-broilers) + (num-laying-hens * emissions-head-laying-hens))
    ; Farms note their emissions each year.
    set annual-emissions lput list ( year ) ( current-emissions ) annual-emissions
    ; Farms calculate their carbon tax liability for the year given their emissions, the tax level, and the year.
    ifelse year < carbon-tax-start-yr [ set current-carbon-tax-liability 0 ] [ set current-carbon-tax-liability round ((current-emissions / 1000) * carbon-tax-per-tonne) ]
    ; Farms note their carbon tax liabilities each year.
    set annual-carbon-tax-liability lput list ( year ) ( current-carbon-tax-liability) annual-carbon-tax-liability
    ; Farms calculate their annual income from crops, livestock, poultry, subsidies, and grants, deducting
    ; carbon tax liabilities at the same time.
    set current-income round farm-income
    ; Farms note their income each year.
    set annual-income lput list ( year ) ( current-income ) annual-income
    ; Add the income to the recent-income list as well. This stores just the income values from the last
    ; five years as we drop the first value in the list each year post-2017.
    if Year <= start-yr + 4 [ set recent-income lput current-income recent-income ]
    if Year > start-yr + 4 [
      set recent-income remove-item 0 recent-income
      set recent-income lput current-income recent-income
    ]
    ; The benchmark income is the mean income of each farm as determined during the first five
    ; years of the simulations. Prior to five year point, the benchmark is set to the mean
    ; annual income since the start of the simulation.
    if Year = start-yr [ set benchmark-income current-income ]
    if Year = start-yr + 1 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income)) / 2 ]
    if Year = start-yr + 2 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income) + (item 1 item 2 annual-income)) / 3 ]
    if Year = start-yr + 3 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income) + (item 1 item 2 annual-income) + (item 1 item 3 annual-income)) / 4 ]
    if Year = start-yr + 4 [ set benchmark-income ((item 1 item 0 annual-income) + (item 1 item 1 annual-income) + (item 1 item 2 annual-income) + (item 1 item 3 annual-income) + (item 1 item 4 annual-income)) / 5 ]
    ; For all years after the initialisation year, farms compare their mean recent-income against
    ; this benchmark income. If it is more than the % specified by the farm-income-viability
    ; parameter below their benchmark income, they will consider ceasing operations. They
    ; do this by drawing a value at random from a uniform distribution between 0 and 1. If the
    ; value is less than the cease-farming-prob slider value, the farm will become inactive.
    if Year > start-yr and mean recent-income < (benchmark-income - abs(farm-income-viability) * (benchmark-income / 100)) [
      if random-float 1 < cease-farming-prob [
        set active? FALSE
        if highlight-inactive-farms = TRUE [
          set size 3
        ]
        set partner-slaughterhouse 0
        set partner-dairy 0
        set current-emissions 0
        set current-income 0
        ask my-links [ die ]
        set color [77 20 106 125]
      ]
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; Run dairy procedures ;
  ;;;;;;;;;;;;;;;;;;;;;;;;

  ask dairies with [ active? = TRUE ] [
    ; Dairies note the volume of raw milk they have for processing in their historical records.
    set processed-rawmilk lput list ( year ) ( supply-rawmilk ) processed-rawmilk
    ; Around 11% of milk supply is wasted during processing. We deduct this first.
    set supply-rawmilk supply-rawmilk * (1 - 0.1105857)
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
    ; Slaughterhouses note the quantity of meat they have for processing in their historical records.
    set processed-beef lput list ( year ) ( supply-beef ) processed-beef
    set processed-pork lput list ( year ) ( supply-pork ) processed-pork
    set processed-lamb lput list ( year ) ( supply-lamb ) processed-lamb
    set processed-chicken lput list ( year ) ( supply-chicken ) processed-chicken
    set processed-eggs lput list ( year ) ( supply-eggs ) processed-eggs
    ; Slaughterhouses convert their meat supplies into meat goods. In this version of the
    ; model, meat is simply converted into meat products on a one-to-one basis.
    set wholesale-stock-beef supply-beef
    set wholesale-stock-pork supply-pork
    set wholesale-stock-lamb supply-lamb
    set wholesale-stock-chicken supply-chicken
    set wholesale-stock-eggs supply-eggs
    ; With all meat converted to meat products, the meat supply of slaughterhouses is reset to 0.
    set supply-beef 0
    set supply-pork 0
    set supply-lamb 0
    set supply-chicken 0
    set supply-eggs 0
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Update CM and PF production quotas to account for changes in the population ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; If there are facilities that are not operating at full capacity, assess whether
  ; they can increase their output to serve an expanded population.

  ; Beef
  if count cm-factories with [ product-type = "Beef" and production-quota != cm-factory-capacity ] > 0 [
    ask one-of cm-factories with [ product-type = "Beef" and production-quota != cm-factory-capacity ] [
      ; If there is sufficient demand to operate at full capacity it will do so.
      let potential-cm-production-beef (count cm-factories with [ product-type = "Beef" ] * cm-factory-capacity)
      if ((potential-cm-production-beef / total-consumption-beef) * 100 < cm-max-share) [
        set production-quota cm-factory-capacity
      ]
      ; Else it will determine the maximum level of production that will not lead
      ; to excess total production, making this its production quota.
      if ((potential-cm-production-beef / total-consumption-beef) * 100 > cm-max-share) [
        set production-quota production-limit-cm-beef - sum [ production-quota ] of other cm-factories with [ product-type = "Beef" ]
      ]
    ]
  ]

  ; Lamb
  if count cm-factories with [ product-type = "Lamb" and production-quota != cm-factory-capacity ] > 0 [
    ask one-of cm-factories with [ product-type = "Lamb" and production-quota != cm-factory-capacity ] [
      ; If there is sufficient demand to operate at full capacity it will do so.
      let potential-cm-production-lamb (count cm-factories with [ product-type = "Lamb" ] * cm-factory-capacity)
      if ((potential-cm-production-lamb / total-consumption-lamb) * 100 < cm-max-share) [
        set production-quota cm-factory-capacity
      ]
      ; Else it will determine the maximum level of production that will not lead
      ; to excess total production, making this its production quota.
      if ((potential-cm-production-lamb / total-consumption-lamb) * 100 > cm-max-share) [
        set production-quota production-limit-cm-lamb - sum [ production-quota ] of other cm-factories with [ product-type = "Lamb" ]
      ]
    ]
  ]

  ; Pork
  if count cm-factories with [ product-type = "Pork" and production-quota != cm-factory-capacity ] > 0 [
    ask one-of cm-factories with [ product-type = "Pork" and production-quota != cm-factory-capacity ] [
      ; If there is sufficient demand to operate at full capacity it will do so.
      let potential-cm-production-pork (count cm-factories with [ product-type = "Pork" ] * cm-factory-capacity)
      if ((potential-cm-production-pork / total-consumption-pork) * 100 < cm-max-share) [
        set production-quota cm-factory-capacity
      ]
      ; Else it will determine the maximum level of production that will not lead
      ; to excess total production, making this its production quota.
      if ((potential-cm-production-pork / total-consumption-pork) * 100 > cm-max-share) [
        set production-quota production-limit-cm-pork - sum [ production-quota ] of other cm-factories with [ product-type = "Pork" ]
      ]
    ]
  ]

  ; Chicken
  if count cm-factories with [ product-type = "Chicken" and production-quota != cm-factory-capacity ] > 0 [
    ask one-of cm-factories with [ product-type = "Chicken" and production-quota != cm-factory-capacity ] [
      ; If there is sufficient demand to operate at full capacity it will do so.
      let potential-cm-production-chicken (count cm-factories with [ product-type = "Chicken" ] * cm-factory-capacity)
      if ((potential-cm-production-chicken / total-consumption-chicken) * 100 < cm-max-share) [
        set production-quota cm-factory-capacity
      ]
      ; Else it will determine the maximum level of production that will not lead
      ; to excess total production, making this its production quota.
      if ((potential-cm-production-chicken / total-consumption-chicken) * 100 > cm-max-share) [
        set production-quota production-limit-cm-chicken - sum [ production-quota ] of other cm-factories with [ product-type = "Chicken" ]
      ]
    ]
  ]

  ; Egg
  if count pf-factories with [ product-type = "Eggs" and production-quota != pf-factory-egg-capacity ] > 0 [
    ask one-of pf-factories with [ product-type = "Eggs" and production-quota != pf-factory-egg-capacity ] [
      ; If there is sufficient demand to operate at full capacity it will do so.
      let potential-pf-production-eggs (count pf-factories with [ product-type = "Eggs" ] * pf-factory-egg-capacity)
      if ((potential-pf-production-eggs / total-consumption-eggs) * 100 < pf-max-share) [
        set production-quota pf-factory-egg-capacity
      ]
      ; Else it will determine the maximum level of production that will not lead
      ; to excess total production, making this its production quota.
      if ((potential-pf-production-eggs / total-consumption-eggs) * 100 > pf-max-share) [
        set production-quota production-limit-pf-eggs - sum [ production-quota ] of other pf-factories with [ product-type = "Eggs" ]
      ]
    ]
  ]

  ; Dairy
  if count pf-factories with [ product-type = "Dairy" and production-quota != pf-factory-dairy-capacity ] > 0 [
    ask one-of pf-factories with [ product-type = "Dairy" and production-quota != pf-factory-dairy-capacity ] [
      ; If there is sufficient demand to operate at full capacity it will do so.
      let total-consumption-rawmilk (total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335000) + (total-consumption-butter * 0.2522000) + (total-consumption-cheese * 6.9140000)
      let potential-pf-production-dairy (count pf-factories with [ product-type = "Dairy" ] * pf-factory-dairy-capacity)
      if ((potential-pf-production-dairy / total-consumption-rawmilk) * 100 < pf-max-share) [
        set production-quota pf-factory-dairy-capacity
      ]
      ; Else it will determine the maximum level of production that will not lead
      ; to excess total production, making this its production quota.
      if ((potential-pf-production-dairy / total-consumption-rawmilk) * 100 > pf-max-share) [
        set production-quota production-limit-pf-rawmilk - sum [ production-quota ] of other pf-factories with [ product-type = "Dairy" ]
      ]
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Account for production of cultured meat facilities ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask cm-factories [
    set wholesale-stock production-quota
    set processed-meat lput list ( year ) ( production-quota ) processed-meat
    set factory-emissions precision (((production-quota * 1000) * emissions-cm-meat) / 1000) 2
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
      set wholesale-stock-milk-cream (production-quota * item 0 raw-milk-division-ratio) / 1
      set wholesale-stock-yoghurt (production-quota * item 1 raw-milk-division-ratio) / 1.13
      set wholesale-stock-butter (production-quota * item 2 raw-milk-division-ratio) / 0.252
      set wholesale-stock-cheese (production-quota * item 3 raw-milk-division-ratio) / 6.91
      set wholesale-stock-eggs 0
      set processed-rawmilk lput list ( year ) ( production-quota ) processed-rawmilk
      set factory-emissions precision (((production-quota * 1000) * emissions-pf-dairy) / 1000) 2
    ]

    if product-type = "Eggs" [
      ; In contrast, eggs production is simply determined by the facilities production quota.
      set wholesale-stock-eggs production-quota
      set wholesale-stock-milk-cream 0
      set wholesale-stock-yoghurt 0
      set wholesale-stock-butter 0
      set wholesale-stock-cheese 0
      set processed-eggs lput list ( year ) ( production-quota ) processed-eggs
      set factory-emissions precision (((production-quota * 1000) * emissions-pf-egg) / 1000) 2
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
    if required-beef > 0 and count cm-factories with [ product-type = "Beef" and wholesale-stock > 0 ] > 0 [
      while [ required-beef > 0 and count cm-factories with [ product-type = "Beef" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ product-type = "Beef" and wholesale-stock > 0 ]
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
  ask cm-factories with [ product-type = "Beef" and wholesale-stock > 0 ] [
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
    if required-pork > 0 and count cm-factories with [ product-type = "Pork" and wholesale-stock > 0 ] > 0 [
      while [ required-pork > 0 and count cm-factories with [ product-type = "Pork" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ product-type = "Pork" and wholesale-stock > 0 ]
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
  ask cm-factories with [ product-type = "Pork" and wholesale-stock > 0 ] [
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
    if required-lamb > 0 and count cm-factories with [ product-type = "Lamb" and wholesale-stock > 0 ] > 0 [
      while [ required-lamb > 0 and count cm-factories with [ product-type = "Lamb" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ product-type = "Lamb" and wholesale-stock > 0 ]
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
  ask cm-factories with [ product-type = "Lamb" and wholesale-stock > 0 ] [
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
    if required-chicken > 0 and count cm-factories with [ product-type = "Chicken" and wholesale-stock > 0 ] > 0 [
      while [ required-chicken > 0 and count cm-factories with [ product-type = "Chicken" and wholesale-stock > 0 ] > 0 ] [
        let cm-factory-to-buy-from one-of cm-factories with [ product-type = "Chicken" and wholesale-stock > 0 ]
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
  ask cm-factories with [ product-type = "Chicken" and wholesale-stock > 0 ] [
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
    ; Calculate the total meat that has been processed in the current year.
    let meat-supply-current (last last processed-beef) + (last last processed-pork) + (last last processed-lamb) + (last last processed-chicken)
    ; If the total is below the min-viable-meat capacity, the slaughterhouse will no longer be viable.
    if meat-supply-current < min-viable-meat [
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
    ; Identify the baseline producer price and production per capita, plus the current production per capita. All
    ; cattle are priced the same per kg meat.
    let baseline-pp filter [ i -> price-baseline-year = item 0 i ] pp-beef-cow-meat
    set baseline-pp item 1 item 0 baseline-pp
    let baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-beef
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    let current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-beef
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    let next-pp baseline-pp + (baseline-pp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new producer price to the pp list for implementation next year.
    set pp-other-cattle-meat lput list (Year + 1) (next-pp) pp-other-cattle-meat
    set pp-beef-cow-meat lput list (Year + 1) (next-pp) pp-beef-cow-meat
    set pp-dairy-cow-meat lput list (Year + 1) (next-pp) pp-dairy-cow-meat

    ; Pork.
    ; Identify the baseline producer price and production per capita, plus the current production per capita.
    set baseline-pp filter [ i -> price-baseline-year = item 0 i ] pp-pig-meat
    set baseline-pp item 1 item 0 baseline-pp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-pork
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-pork
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-pp baseline-pp + (baseline-pp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new producer price to the pp list for implementation next year.
    set pp-pig-meat lput list (Year + 1) (next-pp) pp-pig-meat

    ; Lamb.
    ; Identify the baseline producer price and production per capita, plus the current production per capita.
    set baseline-pp filter [ i -> price-baseline-year = item 0 i ] pp-sheep-meat
    set baseline-pp item 1 item 0 baseline-pp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-lamb
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-lamb
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-pp baseline-pp + (baseline-pp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new producer price to the pp list for implementation next year.
    set pp-sheep-meat lput list (Year + 1) (next-pp) pp-sheep-meat

    ; Chicken.
    ; Identify the baseline producer price and production per capita, plus the current production per capita.
    set baseline-pp filter [ i -> price-baseline-year = item 0 i ] pp-broiler-meat
    set baseline-pp item 1 item 0 baseline-pp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-chicken
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-chicken
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-pp baseline-pp + (baseline-pp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new producer price to the pp list for implementation next year.
    set pp-broiler-meat lput list (Year + 1) (next-pp) pp-broiler-meat

    ; Eggs.
    ; Identify the baseline producer price and production per capita, plus the current production per capita.
    set baseline-pp filter [ i -> price-baseline-year = item 0 i ] pp-eggs
    set baseline-pp item 1 item 0 baseline-pp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-eggs
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-eggs
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-pp baseline-pp + (baseline-pp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new producer price to the pp list for implementation next year.
    set pp-eggs lput list (Year + 1) (next-pp) pp-eggs

    ; Wool.
    ; Identify the baseline producer price and production per capita, plus the current production per capita.
    set baseline-pp filter [ i -> price-baseline-year = item 0 i ] pp-wool
    set baseline-pp item 1 item 0 baseline-pp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-wool
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-wool
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-pp baseline-pp + (baseline-pp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new producer price to the pp list for implementation next year.
    set pp-wool lput list (Year + 1) (next-pp) pp-wool

    ; Raw milk.
    ; Identify the baseline producer price and production per capita, plus the current production per capita.
    set baseline-pp filter [ i -> price-baseline-year = item 0 i ] pp-raw-milk
    set baseline-pp item 1 item 0 baseline-pp
    set baseline-production-per-capita filter [ i -> price-baseline-year = item 0 i ] production-per-capita-rawmilk
    set baseline-production-per-capita item 1 item 0 baseline-production-per-capita
    set current-production-per-capita filter [ i -> Year = item 0 i ] production-per-capita-rawmilk
    set current-production-per-capita item 1 item 0 current-production-per-capita
    ; Determine new price given the current level of production relative to the baseline per capita production
    ; levels. The price-response-ratio slider determines responsiveness of prices to per capita production changes as
    ; a ratio, e.g., a value of 1 means prices go up or down 1% for every 1% change in per capita production.
    set next-pp baseline-pp + (baseline-pp * ((baseline-production-per-capita - current-production-per-capita) / baseline-production-per-capita)) * price-response-ratio
    ; Add the new producer price to the pp list for implementation next year.
    set pp-raw-milk lput list (Year + 1) (next-pp) pp-raw-milk
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Assess whether to construct additional cultured meat production facilities ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Each year, one new cultured meat facility can be built.
  if sim-cm? = TRUE and year >= cm-init-yr [
    ; For each type of meat, check whether biosynthetic production is sufficiently
    ; profitable at current prices given the current carbon tax level and minimum
    ; profit margin required by the CF industry. Also check whether there is already
    ; sufficient production capacity to fulfil all potential market demand for the
    ; biosynthetic product. CM costs decline as total annual production rises
    ; past thresholds set by certain thresholds, leading to modification of the
    ; `cm-cost-` values.
    ; Beef.
    let facility-allowed-beef? TRUE
    if profit-margin-perc-cm-beef < cf-required-profit-margin [
      set facility-allowed-beef? FALSE
    ]
    if ((count cm-factories with [ product-type = "Beef" ] * cm-factory-capacity) / total-consumption-beef) * 100 > cm-max-share [
      set facility-allowed-beef? FALSE
    ]
    ; Pork.
    let facility-allowed-pork? TRUE
    if profit-margin-perc-cm-pork < cf-required-profit-margin [
      set facility-allowed-pork? FALSE
    ]
    if ((count cm-factories with [ product-type = "Pork" ] * cm-factory-capacity) / total-consumption-pork) * 100 > cm-max-share [
      set facility-allowed-pork? FALSE
    ]
    ; Lamb.
    let facility-allowed-lamb? TRUE
    if profit-margin-perc-cm-lamb < cf-required-profit-margin [
      set facility-allowed-lamb? FALSE
    ]
    if ((count cm-factories with [ product-type = "Lamb" ] * cm-factory-capacity) / total-consumption-lamb) * 100 > cm-max-share [
      set facility-allowed-lamb? FALSE
    ]
    ; Chicken.
    let facility-allowed-chicken? TRUE
    if profit-margin-perc-cm-chicken < cf-required-profit-margin [
      set facility-allowed-chicken? FALSE
    ]
    if ((count cm-factories with [ product-type = "Chicken" ] * cm-factory-capacity) / total-consumption-chicken) * 100 > cm-max-share [
      set facility-allowed-chicken? FALSE
    ]
    ; We check whether there are any candidate meats that merit expanded production. If there is more
    ; than one, we will rank them to determine which has the greatest profit margin at current prices
    ; and build a facility for production of that meat.
    if facility-allowed-beef? = TRUE or facility-allowed-pork? = TRUE or facility-allowed-lamb? = TRUE or facility-allowed-chicken? = TRUE [
      ; Create the list of candidate meats.
      let candidate-meats []
      if facility-allowed-beef? = TRUE [
        set candidate-meats lput list (profit-margin-cm-beef) ("Beef") candidate-meats
      ]
      if facility-allowed-pork? = TRUE [
        set candidate-meats lput list (profit-margin-cm-pork) ("Pork") candidate-meats
      ]
      if facility-allowed-lamb? = TRUE [
        set candidate-meats lput list (profit-margin-cm-lamb) ("Lamb") candidate-meats
      ]
      if facility-allowed-chicken? = TRUE [
        set candidate-meats lput list (profit-margin-cm-chicken) ("Chicken") candidate-meats
      ]
      ; Order the list by profit margin.
      set candidate-meats sort-by [[list1 list2] -> first list1 < first list2] candidate-meats
      ; Create a cultured meat factory producing whichever meat has the greatest profit margin.
      create-cm-factories 1 [
        set product-type item 1 last candidate-meats
        set wholesale-stock 0
        set processed-meat []
        ; Specify aesthetics and initial capcity to be utilised.
        if product-type = "Beef" [
          ; If adding a factory will not result in excess production of the product when
          ; the maximum market share is considered, the added factory will operate at
          ; its maximum capacity.
          let potential-cm-production-beef (count cm-factories with [ product-type = "Beef" ]  * cm-factory-capacity)
          if ((potential-cm-production-beef / total-consumption-beef) * 100 < cm-max-share) [
            set production-quota cm-factory-capacity
          ]
          ; Instead, if adding a factory will lead to excess production, the factory will
          ; operate below its maximum capacity, producing whatever is required to bring
          ; total production up to the maximum market share but no higher.
          if ((potential-cm-production-beef / total-consumption-beef) * 100 > cm-max-share) [
            set production-quota production-limit-cm-beef - sum [ production-quota ] of other cm-factories with [ product-type = "Beef" ]
          ]
          set ycor -105
          set xcor 220 - (20 * count cm-factories with [ product-type = "Beef" ])
        ]
        if product-type = "Pork" [
          ; If adding a factory will not result in excess production of the product when
          ; the maximum market share is considered, the added factory will operate at
          ; its maximum capacity.
          let potential-cm-production-pork (count cm-factories with [ product-type = "Pork" ]  * cm-factory-capacity)
          if ((potential-cm-production-pork / total-consumption-pork) * 100 < cm-max-share) [
            set production-quota cm-factory-capacity
          ]
          ; Instead, if adding a factory will lead to excess production, the factory will
          ; operate below its maximum capacity, producing whatever is required to bring
          ; total production up to the maximum market share but no higher.
          if ((potential-cm-production-pork / total-consumption-pork) * 100 > cm-max-share) [
            set production-quota production-limit-cm-pork - sum [ production-quota ] of other cm-factories with [ product-type = "Pork" ]
          ]
          set ycor -120
          set xcor 220 - (20 * count cm-factories with [ product-type = "Pork" ])
        ]
        if product-type = "Lamb" [
          ; If adding a factory will not result in excess production of the product when
          ; the maximum market share is considered, the added factory will operate at
          ; its maximum capacity.
          let potential-cm-production-lamb (count cm-factories with [ product-type = "Lamb" ]  * cm-factory-capacity)
          if ((potential-cm-production-lamb / total-consumption-lamb) * 100 < cm-max-share) [
            set production-quota cm-factory-capacity
          ]
          ; Instead, if adding a factory will lead to excess production, the factory will
          ; operate below its maximum capacity, producing whatever is required to bring
          ; total production up to the maximum market share but no higher.
          if ((potential-cm-production-lamb / total-consumption-lamb) * 100 > cm-max-share) [
            set production-quota production-limit-cm-lamb - sum [ production-quota ] of other cm-factories with [ product-type = "Lamb" ]
          ]
          set ycor -135
          set xcor 220 - (20 * count cm-factories with [ product-type = "Lamb" ])
        ]
        if product-type = "Chicken" [
          ; If adding a factory will not result in excess production of the product when
          ; the maximum market share is considered, the added factory will operate at
          ; its maximum capacity.
          let potential-cm-production-chicken (count cm-factories with [ product-type = "Chicken" ]  * cm-factory-capacity)
          if ((potential-cm-production-chicken / total-consumption-chicken) * 100 < cm-max-share) [
            set production-quota cm-factory-capacity
          ]
          ; Instead, if adding a factory will lead to excess production, the factory will
          ; operate below its maximum capacity, producing whatever is required to bring
          ; total production up to the maximum market share but no higher.
          if ((potential-cm-production-chicken / total-consumption-chicken) * 100 > cm-max-share) [
            set production-quota production-limit-cm-chicken - sum [ production-quota ] of other cm-factories with [ product-type = "Chicken" ]
          ]
          set ycor -150
          set xcor 220 - (20 * count cm-factories with [ product-type = "Chicken" ])
        ]
        set color [204 116 94]
        set shape "biosynth-factory"
        set size 20
      ]
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Assess whether to construct additional biosynthetic liquid production facilities ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Each year, one new egg and one new milk biosynthetic liquid facility can be built.
  if sim-pf? = TRUE and year >= pf-init-yr [
    ; For each type of biosynthetic liquid, check whether biosynthetic production is profitable at
    ; current prices and given the current carbon tax level. Also check whether there is already
    ; sufficient production capacity to fulfil all potential market demand for the biosynthetic
    ; product. BL costs decline as total annual production rises past thresholds set by certain
    ; thresholds, leading to modification of the `pf-cost-` values.
    ; Dairy.
    let facility-allowed-dairy? TRUE
    if profit-margin-perc-pf-raw-milk < cf-required-profit-margin [
      set facility-allowed-dairy? FALSE
    ]
    let total-consumption-rawmilk (total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335000) + (total-consumption-butter * 0.2522000) + (total-consumption-cheese * 6.9140000)
    if ((count pf-factories with [ product-type = "Dairy" ] * pf-factory-dairy-capacity) / total-consumption-rawmilk) * 100 > pf-max-share [
      set facility-allowed-dairy? FALSE
    ]
    ; If milk facilities are allowed, we will build one.
    if facility-allowed-dairy? = TRUE [
      ; Create a biosynthetic liquid factory producing milk and converting it into dairy products.
      create-pf-factories 1 [
        set product-type "Dairy"
        ; If adding a factory will not result in excess production of the product when
        ; the maximum market share is considered, the added factory will operate at
        ; its maximum capacity.
        let potential-pf-production-dairy (count pf-factories with [ product-type = "Dairy" ]  * pf-factory-dairy-capacity)
        if ((potential-pf-production-dairy / total-consumption-rawmilk) * 100 < pf-max-share) [
          set production-quota pf-factory-dairy-capacity
        ]
        ; Instead, if adding a factory will lead to excess production, the factory will
        ; operate below its maximum capacity, producing whatever is required to bring
        ; total production up to the maximum market share but no higher.
        if ((potential-pf-production-dairy / total-consumption-rawmilk) * 100 > pf-max-share) [
          set production-quota production-limit-pf-rawmilk - sum [ production-quota ] of other pf-factories with [ product-type = "Dairy" ]
        ]
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
    if profit-margin-perc-pf-eggs < cf-required-profit-margin [
      set facility-allowed-eggs? FALSE
    ]
    if ((count pf-factories with [ product-type = "Eggs" ] * pf-factory-egg-capacity) / total-consumption-eggs) * 100 > pf-max-share [
      set facility-allowed-eggs? FALSE
    ]
    ; If egg facilities are allowed, we will build one.
    if facility-allowed-eggs? = TRUE [
      ; Create a biosynthetic liquid factory producing egg.
      create-pf-factories 1 [
        set product-type "Eggs"
        let potential-pf-production-eggs (count pf-factories with [ product-type = "Eggs" ]  * pf-factory-egg-capacity)
        ; If adding a factory will not result in excess production of the product when
        ; the maximum market share is considered, the added factory will operate at
        ; its maximum capacity.
        if ((potential-pf-production-eggs / total-consumption-eggs) * 100 < pf-max-share) [
          set production-quota pf-factory-egg-capacity
        ]
        ; Instead, if adding a factory will lead to excess production, the factory will
        ; operate below its maximum capacity, producing whatever is required to bring
        ; total production up to the maximum market share but no higher.
        if ((potential-pf-production-eggs / total-consumption-eggs) * 100 > pf-max-share) [
          set production-quota production-limit-pf-eggs - sum [ production-quota ] of other pf-factories with [ product-type = "Eggs" ]
        ]
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
  ; Check if we have reached the specified end yr for the simulation.
;  if year = sim-end-yr [
;    stop
;    if play-end-sound? = TRUE [
;      repeat 5 [ beep wait 0.1 ]
;    ]
;  ]
  ; Advance a year.
  set year year + 1
  tick
  if play-step-sound? = TRUE [
    repeat 3 [ beep wait 0.1 ]
  ]
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

to-report specialist-cattle-farms
  report count farms with [ active? = TRUE and farm-type = "Specialist cattle" ]
end

to-report specialist-sheep-farms
  report count farms with [ active? = TRUE and farm-type = "Specialist sheep" ]
end

to-report specialist-pig-farms
  report count farms with [ active? = TRUE and farm-type = "Specialist pig" ]
end

to-report specialist-broiler-farms
  report count farms with [ active? = TRUE and farm-type = "Specialist broiler" ]
end

to-report specialist-laying-hen-farms
  report count farms with [ active? = TRUE and farm-type = "Specialist laying hens" ]
end

to-report specialist-fodder-silage-farms
  report count farms with [ active? = TRUE and farm-type = "Specialist fodder/silage" ]
end

to-report specialist-arable-horticulture-farms
  report count farms with [ active? = TRUE and farm-type = "Specialist arable/horticulture" ]
end

to-report combined-cattle-grain-farms
  report count farms with [ active? = TRUE and farm-type = "Combined cattle & grain" ]
end

to-report combined-cattle-sheep-farms
  report count farms with [ active? = TRUE and farm-type = "Combined cattle & sheep" ]
end

to-report other-mixed-farms
  report count farms with [ active? = TRUE and farm-type = "Other mixed" ]
end

to-report no-activity-farms
  report count farms with [ active? = TRUE and farm-type = "No activities reported" ]
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
end

to-report farm-production-dairy-cow-beef
  let my-production 0
  if num-dairy-cows > 0 [
    let my-fylke-id fylke-id
    let local-yield-dairy-cows 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield-dairy-cows filter [ i -> Year = item 0 i ] yield-dairy-cow-meat
      set local-yield-dairy-cows item 1 item 0 local-yield-dairy-cows
    ]
    set my-production (local-yield-dairy-cows * num-dairy-cows)
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report precision my-production 4
end

to-report farm-production-beef-cow-beef
  let my-production 0
  if num-beef-cows > 0 [
    let my-fylke-id fylke-id
    let local-yield-beef-cows 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield-beef-cows filter [ i -> Year = item 0 i ] yield-beef-cow-meat
      set local-yield-beef-cows item 1 item 0 local-yield-beef-cows
    ]
    set my-production (local-yield-beef-cows * num-beef-cows)
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report precision my-production 4
end

to-report farm-production-other-cattle-beef
  let my-production 0
  if num-other-cattle > 0 [
    let my-fylke-id fylke-id
    let local-yield-other-cattle 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield-other-cattle filter [ i -> Year = item 0 i ] yield-other-cattle-meat
      set local-yield-other-cattle item 1 item 0 local-yield-other-cattle
    ]
    set my-production (local-yield-other-cattle * num-other-cattle)
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report precision my-production 4
end

to-report farm-production-beef
  let my-production farm-production-dairy-cow-beef + farm-production-beef-cow-beef + farm-production-other-cattle-beef
  report my-production
end

to-report farm-production-pork
  let my-production 0
  if num-pigs > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-pig-meat
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-pigs
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report precision my-production 4
end

to-report farm-production-lamb
  let my-production 0
  if num-sheep > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-sheep-meat
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-sheep
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report precision my-production 4
end

to-report farm-production-chicken
  let my-production 0
  if num-broilers > 0 [
    let my-fylke-id fylke-id
    let local-yield 0
    ask fylker with [ fylke-id = my-fylke-id ] [
      ; Extract the local yield for the current year.
      set local-yield filter [ i -> Year = item 0 i ] yield-broiler-meat
      set local-yield item 1 item 0 local-yield
    ]
    set my-production local-yield * num-broilers
  ]
  ; If inactive, production is reset to zero.
  if active? = FALSE [ set my-production 0 ]
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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
  report precision my-production 4
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

to-report total-farm-production-dairy-cow-beef
  report sum [ farm-production-dairy-cow-beef ] of farms with [active? = TRUE]
end

to-report total-farm-production-beef-cow-beef
  report sum [ farm-production-beef-cow-beef ] of farms with [active? = TRUE]
end

to-report total-farm-production-other-cattle-beef
  report sum [ farm-production-other-cattle-beef ] of farms with [active? = TRUE]
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
  report sum [ production-quota ] of cm-factories with [product-type = "Beef"]
end

to-report total-factory-production-pork
  report sum [ production-quota ] of cm-factories with [product-type = "Pork"]
end

to-report total-factory-production-lamb
  report sum [ production-quota ] of cm-factories with [product-type = "Lamb"]
end

to-report total-factory-production-chicken
  report sum [ production-quota ] of cm-factories with [product-type = "Chicken"]
end

to-report total-factory-production-meat
  report total-factory-production-beef + total-factory-production-pork + total-factory-production-lamb + total-factory-production-chicken
end

to-report total-factory-production-eggs
  report sum [ production-quota ] of pf-factories with [product-type = "Eggs"]
end

to-report total-factory-production-raw-milk
  report sum [ production-quota ] of pf-factories with [product-type = "Dairy"]
end

to-report total-factory-production-pf
  report total-factory-production-eggs + total-factory-production-raw-milk
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report producer price ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report current-pp-barley
  let national-pp-barley filter [ i -> Year = item 0 i ] pp-barley
  set national-pp-barley item 1 item 0 national-pp-barley
  report national-pp-barley
end

to-report current-pp-oats
  let national-pp-oats filter [ i -> Year = item 0 i ] pp-oats
  set national-pp-oats item 1 item 0 national-pp-oats
  report national-pp-oats
end

to-report current-pp-wheat
  let national-pp-wheat filter [ i -> Year = item 0 i ] pp-wheat
  set national-pp-wheat item 1 item 0 national-pp-wheat
  report national-pp-wheat
end

to-report current-pp-rye-triticale
  let national-pp-rye-triticale filter [ i -> Year = item 0 i ] pp-rye-triticale
  set national-pp-rye-triticale item 1 item 0 national-pp-rye-triticale
  report national-pp-rye-triticale
end

to-report current-pp-oilseeds
  let national-pp-oilseeds filter [ i -> Year = item 0 i ] pp-oilseeds
  set national-pp-oilseeds item 1 item 0 national-pp-oilseeds
  report national-pp-oilseeds
end

to-report current-pp-potatoes
  let national-pp-potatoes filter [ i -> Year = item 0 i ] pp-potatoes
  set national-pp-potatoes item 1 item 0 national-pp-potatoes
  report national-pp-potatoes
end

to-report current-pp-vegetables
  let national-pp-vegetables filter [ i -> Year = item 0 i ] pp-vegetables
  set national-pp-vegetables item 1 item 0 national-pp-vegetables
  report national-pp-vegetables
end

to-report current-pp-fodder-silage
  let national-pp-fodder-silage filter [ i -> Year = item 0 i ] pp-fodder-silage
  set national-pp-fodder-silage item 1 item 0 national-pp-fodder-silage
  report national-pp-fodder-silage
end

to-report current-pp-other-crops
  let national-pp-other-crops filter [ i -> Year = item 0 i ] pp-other-crops
  set national-pp-other-crops item 1 item 0 national-pp-other-crops
  report national-pp-other-crops
end

to-report current-pp-pome-stone-fruit
  let national-pp-pome-stone-fruit filter [ i -> Year = item 0 i ] pp-pome-stone-fruit
  set national-pp-pome-stone-fruit item 1 item 0 national-pp-pome-stone-fruit
  report national-pp-pome-stone-fruit
end

to-report current-pp-berries
  let national-pp-berries filter [ i -> Year = item 0 i ] pp-berries
  set national-pp-berries item 1 item 0 national-pp-berries
  report national-pp-berries
end

to-report current-pp-cattle-meat
  let national-pp-cattle filter [ i -> Year = item 0 i ] pp-other-cattle-meat
  set national-pp-cattle item 1 item 0 national-pp-cattle
  report precision national-pp-cattle 0
end

to-report current-pp-pig-meat
  let national-pp-pigs filter [ i -> Year = item 0 i ] pp-pig-meat
  set national-pp-pigs item 1 item 0 national-pp-pigs
  report precision national-pp-pigs 0
end

to-report current-pp-sheep-meat
  let national-pp-sheep filter [ i -> Year = item 0 i ] pp-sheep-meat
  set national-pp-sheep item 1 item 0 national-pp-sheep
  report precision national-pp-sheep 0
end

to-report current-pp-broiler-meat
  let national-pp-broiler filter [ i -> Year = item 0 i ] pp-broiler-meat
  set national-pp-broiler item 1 item 0 national-pp-broiler
  report precision national-pp-broiler 0
end

to-report current-pp-eggs
  let national-pp-eggs filter [ i -> Year = item 0 i ] pp-eggs
  set national-pp-eggs item 1 item 0 national-pp-eggs
  report precision national-pp-eggs 0
end

to-report current-pp-raw-milk
  let national-pp-raw-milk filter [ i -> Year = item 0 i ] pp-raw-milk
  set national-pp-raw-milk item 1 item 0 national-pp-raw-milk
  report precision national-pp-raw-milk 0
end

to-report current-pp-wool
  let national-pp-wool filter [ i -> Year = item 0 i ] pp-wool
  set national-pp-wool item 1 item 0 national-pp-wool
  report precision national-pp-wool 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report profit margins of CF foods in NOK/t ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report profit-margin-cm-beef
  report precision (current-pp-cattle-meat - ((cm-cost-beef * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) 0
end

to-report profit-margin-cm-lamb
  report precision (current-pp-sheep-meat - ((cm-cost-lamb * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) 0
end

to-report profit-margin-cm-pork
  report precision (current-pp-pig-meat - ((cm-cost-pork * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) 0
end

to-report profit-margin-cm-chicken
  report precision (current-pp-broiler-meat - ((cm-cost-chicken * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) 0
end

to-report profit-margin-pf-eggs
  report precision (current-pp-eggs - ((pf-cost-eggs * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)) + (emissions-pf-egg * carbon-tax-per-tonne))) 0
end

to-report profit-margin-pf-raw-milk
  report precision (current-pp-raw-milk - ((pf-cost-dairy * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)) + (emissions-pf-dairy * carbon-tax-per-tonne))) 0
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report profit margins of CF foods as a % ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report profit-margin-perc-cm-beef
  report precision (((current-pp-cattle-meat - ((cm-cost-beef * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) / current-pp-cattle-meat) * 100) 2
end

to-report profit-margin-perc-cm-lamb
  report precision (((current-pp-sheep-meat - ((cm-cost-lamb * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) / current-pp-sheep-meat) * 100) 2
end

to-report profit-margin-perc-cm-pork
  report precision (((current-pp-pig-meat - ((cm-cost-pork * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) / current-pp-pig-meat) * 100) 2
end

to-report profit-margin-perc-cm-chicken
  report precision (((current-pp-broiler-meat - ((cm-cost-chicken * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))) / current-pp-broiler-meat) * 100) 2
end

to-report profit-margin-perc-pf-eggs
  report precision (((current-pp-eggs - ((pf-cost-eggs * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)) + (emissions-pf-egg * carbon-tax-per-tonne))) / current-pp-eggs) * 100) 2
end

to-report profit-margin-perc-pf-raw-milk
  report precision (((current-pp-raw-milk - ((pf-cost-dairy * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)) + (emissions-pf-dairy * carbon-tax-per-tonne))) / current-pp-raw-milk) * 100) 2
end

;;;;;;;;;;;;;;;;;;;;;;;;;
; Calculate farm income ;
;;;;;;;;;;;;;;;;;;;;;;;;;

; For animal products, send the production to a dairy or slaughterhouse. Initially use the nearest facility, but update this later to account for capacity.
; Calculate the mean income each year for the first five years, updating it for each of these years, making this the baseline against which farms judge viability.
to-report farm-income
  let income-barley (farm-production-barley * current-pp-barley)
  let income-oats (farm-production-oats * current-pp-oats)
  let income-wheat (farm-production-wheat * current-pp-wheat)
  let income-rye-triticale (farm-production-rye-triticale * current-pp-rye-triticale)
  let income-oilseeds (farm-production-oilseeds * current-pp-oilseeds)
  let income-potatoes (farm-production-potatoes * current-pp-potatoes)
  let income-vegetables (farm-production-vegetables * current-pp-vegetables)
  let income-fodder-silage (farm-production-fodder-silage * current-pp-fodder-silage)
  let income-other-crops (farm-production-other-crops * current-pp-other-crops)
  let income-pome-stone-fruit (farm-production-orchards * current-pp-pome-stone-fruit)
  let income-berries (farm-production-berries * current-pp-berries)
  let income-beef (farm-production-beef * current-pp-cattle-meat)
  let income-pork (farm-production-pork * current-pp-pig-meat)
  let income-lamb (farm-production-lamb * current-pp-sheep-meat)
  let income-chicken (farm-production-chicken * current-pp-broiler-meat)
  let income-eggs (farm-production-eggs * current-pp-eggs)
  let income-wool (farm-production-wool * current-pp-wool)
  let income-raw-milk (farm-production-raw-milk * current-pp-raw-milk)
  let subsidy-income (subsidy-nonlivestock + subsidy-livestock + subsidy-milk)
  let my-total-farm-income (income-barley + income-oats + income-wheat + income-rye-triticale + income-oilseeds + income-potatoes + income-vegetables + income-fodder-silage + income-other-crops + income-pome-stone-fruit + income-berries + income-beef + income-lamb + income-pork + income-chicken + income-eggs + income-wool + income-raw-milk + subsidy-income)
  ; If a carbon tax is in operation, adjust the total farm income to account for carbon tax liabilities.
  ; We convert farm emissions from kg to tonnes as the carbon tax is set per tonne.
  if year >= carbon-tax-start-yr [
    set my-total-farm-income my-total-farm-income - current-carbon-tax-liability
  ;  set my-total-farm-income my-total-farm-income - ((current-emissions / 1000) * carbon-tax-per-tonne)
  ]
  if  active? = FALSE [ set my-total-farm-income 0 ]
  report my-total-farm-income
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report the effective CF product quotas for the current year ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report production-limit-cm-beef
  report precision ((total-consumption-beef / 100) * cm-max-share) 2
end

to-report production-limit-cm-lamb
  report precision ((total-consumption-lamb / 100) * cm-max-share) 2
end

to-report production-limit-cm-pork
  report precision ((total-consumption-pork / 100) * cm-max-share) 2
end

to-report production-limit-cm-chicken
  report precision ((total-consumption-chicken / 100) * cm-max-share) 2
end

to-report production-limit-pf-rawmilk
  let total-consumption-rawmilk (total-consumption-milk-cream * 1) + (total-consumption-yoghurt * 1.1335000) + (total-consumption-butter * 0.2522000) + (total-consumption-cheese * 6.9140000)
  report precision ((total-consumption-rawmilk / 100) * pf-max-share) 2
end

to-report production-limit-pf-eggs
  report precision ((total-consumption-eggs / 100) * pf-max-share) 2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report total CO2-equiv. emissions by activity in kt                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report total-emissions-barley
  report precision ((total-ha-barley * emissions-ha-barley) / 1000000) 2
end

to-report total-emissions-oats
  report precision ((total-ha-oats * emissions-ha-oats) / 1000000) 2
end

to-report total-emissions-wheat
  report precision ((total-ha-wheat * emissions-ha-wheat) / 1000000) 2
end

to-report total-emissions-rye-triticale
  report precision ((total-ha-rye-triticale * emissions-ha-rye-triticale) / 1000000) 2
end

to-report total-emissions-oilseeds
  report precision ((total-ha-oilseeds * emissions-ha-oilseeds) / 1000000) 2
end

to-report total-emissions-potatoes
  report precision ((total-ha-potatoes * emissions-ha-potatoes) / 1000000) 2
end

to-report total-emissions-vegetables
  report precision ((total-ha-vegetables * emissions-ha-vegetables) / 1000000) 2
end

to-report total-emissions-fodder-silage
  report precision ((total-ha-fodder-silage * emissions-ha-fodder-silage) / 1000000) 2
end

to-report total-emissions-other-crops
  report precision ((total-ha-other-crops * emissions-ha-other-crops) / 1000000) 2
end

to-report total-emissions-orchards
  report precision ((total-ha-orchards * emissions-ha-orchards) / 1000000) 2
end

to-report total-emissions-berries
  report precision ((total-ha-berries * emissions-ha-berries) / 1000000) 2
end

to-report total-emissions-dairy-cows
  report precision ((total-dairy-cows * emissions-head-dairy-cows) / 1000000) 2
end

to-report total-emissions-beef-cows
  report precision ((total-beef-cows * emissions-head-beef-cows) / 1000000) 2
end

to-report total-emissions-other-cattle
  report precision ((total-other-cattle * emissions-head-other-cattle) / 1000000) 2
end

to-report total-emissions-sheep
  report precision ((total-sheep * emissions-head-sheep) / 1000000) 2
end

to-report total-emissions-pigs
  report precision ((total-pigs * emissions-head-pigs) / 1000000) 2
end

to-report total-emissions-broilers
  report precision ((total-broilers * emissions-head-broilers) / 1000000) 2
end

to-report total-emissions-laying-hens
  report precision ((total-laying-hens * emissions-head-laying-hens) / 1000000) 2
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
  report precision (sum [ factory-emissions ] of cm-factories / 1000) 2
end

to-report total-emissions-cm-beef
  report precision (sum [ factory-emissions ] of cm-factories with [ product-type = "Beef" ] / 1000) 2
end

to-report total-emissions-cm-lamb
  report precision (sum [ factory-emissions ] of cm-factories with [ product-type = "Lamb" ] / 1000) 2
end

to-report total-emissions-cm-pork
  report precision (sum [ factory-emissions ] of cm-factories with [ product-type = "Pork" ] / 1000) 2
end

to-report total-emissions-cm-chicken
  report precision (sum [ factory-emissions ] of cm-factories with [ product-type = "Chicken" ] / 1000) 2
end

to-report total-emissions-pf-dairy
  report precision (sum [ factory-emissions ] of pf-factories with [ product-type = "Dairy" ] / 1000) 2
end

to-report total-emissions-pf-egg
  report precision (sum [ factory-emissions ] of pf-factories with [ product-type = "Eggs" ] / 1000) 2
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

; Conventional beef has an emissions coefficient of x kg CO2-eq. per kg.
to-report av-emissions-kg-beef
  ; Total emissions from farmed beef in kg.
  let emissions-kg-conventional ((total-farm-production-dairy-cow-beef * 1000) * 27.657) + ((total-farm-production-beef-cow-beef * 1000) * 46.535) + ((total-farm-production-other-cattle-beef * 1000) * 54.346)
  ; Total emissions from CM beef in kg.
  let emissions-kg-cm total-emissions-cm-beef * 1000000
  ; Total production of beef in kg.
  let total-production-kg (total-farm-production-beef + total-factory-production-beef) * 1000
  ; Total combined emissions divided by total combined production.
  report (emissions-kg-conventional + emissions-kg-cm) / total-production-kg
end

; Conventional lamb has an emissions coefficient of 37.207 kg CO2-eq. per kg.
to-report av-emissions-kg-lamb
  ; Total emissions from farmed lamb in kg.
  let emissions-kg-conventional (total-farm-production-lamb * 1000) * 37.207
  ; Total emissions from CM lamb in kg.
  let emissions-kg-cm total-emissions-cm-lamb * 1000000
  ; Total production of lamb in kg.
  let total-production-kg (total-farm-production-lamb + total-factory-production-lamb) * 1000
  ; Total combined emissions divided by total combined production.
  report (emissions-kg-conventional + emissions-kg-cm) / total-production-kg
end

; Conventional pork has an emissions coefficient of 2.022 kg CO2-eq. per kg.
to-report av-emissions-kg-pork
  ; Total emissions from farmed pork in kg.
  let emissions-kg-conventional (total-farm-production-pork * 1000) * 2.022
  ; Total emissions from CM pork in kg.
  let emissions-kg-cm total-emissions-cm-pork * 1000000
  ; Total production of pork in kg.
  let total-production-kg (total-farm-production-pork + total-factory-production-pork) * 1000
  ; Total combined emissions divided by total combined production.
  report (emissions-kg-conventional + emissions-kg-cm) / total-production-kg
end

; Conventional chicken has an emissions coefficient of 0.69 kg CO2-eq. per kg.
to-report av-emissions-kg-chicken
  ; Total emissions from farmed chicken in kg.
  let emissions-kg-conventional (total-farm-production-chicken * 1000) * 0.69
  ; Total emissions from CM chicken in kg.
  let emissions-kg-cm total-emissions-cm-chicken * 1000000
  ; Total production of chicken in kg.
  let total-production-kg (total-farm-production-chicken + total-factory-production-chicken) * 1000
  ; Total combined emissions divided by total combined production.
  report (emissions-kg-conventional + emissions-kg-cm) / total-production-kg
end

; Conventional dairy has an emissions coefficient of 1.139 kg CO2-eq. per kg.
; PF dairy has an emissions coefficient set by emissions-pf-dairy.
to-report av-emissions-kg-dairy
  ; Total emissions from farmed dairy in kg.
  let emissions-kg-conventional (total-farm-production-raw-milk * 1000) * 1.139
  ; Total emissions from PF dairy in kg.
  let emissions-kg-pf total-emissions-pf-dairy * 1000000
  ; Total production of dairy in kg.
  let total-production-kg (total-farm-production-raw-milk + total-factory-production-raw-milk) * 1000
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
  let total-production-kg (total-farm-production-eggs + total-factory-production-eggs) * 1000
  ; Total combined emissions divided by total combined production.
  report (emissions-kg-conventional + emissions-kg-pf) / total-production-kg
end

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report total farm income each year ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Report total income of farms in billions of NOK.
to-report total-farm-income
  report sum [ current-income ] of farms with [ active? = TRUE ]
end
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
1
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
1723
23
1922
143
Population
Tick
Million
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
1723
145
1922
265
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
1723
10
1873
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
1
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
1
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
1817
878
2016
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
1817
1244
2016
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
1817
1122
2016
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
1817
1000
2016
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
1817
1366
2016
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
1817
1488
2016
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
1817
1610
2016
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
1817
1732
2016
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
1817
1854
2016
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
1817
756
2016
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
Producer price beef
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
"default" 1.0 0 -16777216 true "" "plot current-pp-cattle-meat"
"CM beef cost" 1.0 0 -6759204 true "" "plot ((cm-cost-beef * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))"

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
Producer price lamb
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
"default" 1.0 0 -16777216 true "" "plot current-pp-sheep-meat"
"CM lamb cost" 1.0 0 -6759204 true "" "plot ((cm-cost-lamb * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))"

PLOT
1415
1122
1614
1242
Producer price pork
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
"default" 1.0 0 -16777216 true "" "plot current-pp-pig-meat"
"CM pork cost" 1.0 0 -6759204 true "" "plot ((cm-cost-pork * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))"

PLOT
1415
1244
1614
1364
Producer price chicken
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
"default" 1.0 0 -16777216 true "" "plot current-pp-broiler-meat"
"CM chicken cost" 1.0 0 -6759204 true "" "plot ((cm-cost-chicken * efficiency-gain-multiplier ^ floor(total-factory-production-meat / efficiency-step-int-nonmilk)) + (emissions-cm-meat * carbon-tax-per-tonne))"

PLOT
1415
1366
1614
1486
Producer price eggs
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
"default" 1.0 0 -16777216 true "" "plot current-pp-eggs"
"PF egg cost" 1.0 0 -6759204 true "" "plot ((pf-cost-eggs * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)) + (emissions-pf-egg * carbon-tax-per-tonne))"

PLOT
1415
1488
1614
1608
Producer price raw milk
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
"default" 1.0 0 -16777216 true "" "plot current-pp-raw-milk"
"PF raw milk cost" 1.0 0 -6759204 true "" "plot ((pf-cost-dairy * efficiency-gain-multiplier ^ floor(total-factory-production-pf / efficiency-step-int-milk)) + (emissions-pf-dairy * carbon-tax-per-tonne))"

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
0
1
-1000

PLOT
2018
878
2217
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
2018
1000
2217
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
2018
1122
2217
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
2018
1244
2217
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
2018
1366
2217
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
2018
1488
2217
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
2018
756
2217
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
1723
389
1922
509
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
1723
511
1922
631
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
1

CHOOSER
539
682
739
727
cm-scenario
cm-scenario
"Scenario 1" "Scenario 2" "Scenario 3" "Scenario 4" "Scenario 5" "Scenario 6" "Scenario 7" "Scenario 8"
6

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
2219
878
2418
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
2219
1000
2418
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
2219
1122
2418
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
2219
1244
2418
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
"default" 1.0 0 -16777216 true "" "plot total-factory-production-beef"
"production-limit" 1.0 0 -3508570 true "" "plot production-limit-cm-beef"

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
"default" 1.0 0 -16777216 true "" "plot total-factory-production-lamb"
"production-limit" 1.0 0 -3508570 true "" "plot production-limit-cm-lamb"

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
"default" 1.0 0 -16777216 true "" "plot total-factory-production-pork"
"production-limit" 1.0 0 -3508570 true "" "plot production-limit-cm-pork"

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
"default" 1.0 0 -16777216 true "" "plot total-factory-production-chicken"
"production-limit" 1.0 0 -3508570 true "" "plot production-limit-cm-chicken"

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
500
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
2420
878
2619
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
2420
1000
2619
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
2420
1122
2619
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
2420
1244
2619
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
"default" 1.0 0 -16777216 true "" "plot total-factory-production-eggs"
"production-limit" 1.0 0 -3508570 true "" "plot production-limit-pf-eggs"

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
2219
1366
2418
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
2420
1366
2619
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
1924
389
2123
509
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
1924
511
2123
631
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
2125
511
2324
631
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
316
895
334
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
618
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
"default" 1.0 0 -16777216 true "" "plot total-factory-production-raw-milk"
"production-limit" 1.0 0 -3508570 true "" "plot production-limit-pf-rawmilk"

PLOT
2219
1488
2418
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
2420
1488
2619
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
"default" 1.0 0 -16777216 true "" "plot total-factory-production-meat + total-factory-production-pf"

PLOT
2219
756
2418
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
2420
756
2619
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
680000.0
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
"default" 1.0 0 -16777216 true "" "plot total-farm-production-beef + total-factory-production-beef"

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
"default" 1.0 0 -16777216 true "" "plot total-farm-production-lamb + total-factory-production-lamb"

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
"default" 1.0 0 -16777216 true "" "plot total-farm-production-pork + total-factory-production-pork"

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
"default" 1.0 0 -16777216 true "" "plot total-farm-production-chicken + total-factory-production-chicken"

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
"default" 1.0 0 -16777216 true "" "plot total-farm-production-eggs + total-factory-production-eggs"

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
"default" 1.0 0 -16777216 true "" "plot total-farm-production-raw-milk + total-factory-production-raw-milk"

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
"default" 1.0 0 -16777216 true "" "plot total-farm-production-animal-products + total-factory-production-meat + total-factory-production-pf"

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
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-animal-products + total-factory-production-meat + total-factory-production-pf) / total-population) * 1000"

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
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-beef + total-factory-production-beef) / total-population) * 1000"

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
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-lamb + total-factory-production-lamb) / total-population) * 1000"

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
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-pork + total-factory-production-pork) / total-population) * 1000"

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
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-chicken + total-factory-production-chicken) / total-population) * 1000"

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
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-eggs + total-factory-production-eggs) / total-population) * 1000"

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
"default" 1.0 0 -16777216 true "" "plot ((total-farm-production-raw-milk + total-factory-production-raw-milk) / total-population) * 1000"

TEXTBOX
1205
415
1568
457
Emissions by activity inc. energy & LULUCF (agricultural emission coefficients based on Table 12 Mittenzwei and Prestvik (2022))
11
0.0
1

MONITOR
1406
493
1560
538
kt CO2-eq. livestock
precision total-emissions-livestock 2
17
1
11

MONITOR
1406
446
1560
491
kt CO2-eq. crops
precision total-emissions-crops 2
17
1
11

MONITOR
1406
682
1560
727
kt CO2-eq. total
precision total-emissions 2
17
1
11

MONITOR
1562
446
1716
491
% emissions crops
precision emissions-share-crops 2
17
1
11

MONITOR
1562
493
1716
538
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
446
1404
727
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
"All crops" 1.0 0 -13210332 true "" "plot total-emissions-crops"
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
1

TEXTBOX
1408
40
1714
194
Emission coefficients for conventionally produced products in Norway are as follows (Source: Mittenzwei and Prestvik, 2022 p.21):\n\n• Chicken: 0.69\n• Pork: 2.022\n• Lamb: 37.207\n• Beef: 27.657 (d. cow); 46.535 (b. cow); 54.346 (other)\n• Raw milk: 1.139\n• Eggs: 0.777
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
1205
240
1463
282
Average emissions from domestic production (kg CO2-eq. per kg)
11
0.0
1

MONITOR
1406
634
1560
679
kt CO2-eq. PF egg
precision total-emissions-pf-egg 2
17
1
11

MONITOR
1406
587
1560
632
kt CO2-eq. PF dairy
precision total-emissions-pf-dairy 2
17
1
11

MONITOR
1406
540
1560
585
kt CO2-eq. CM meat
precision total-emissions-cm 2
17
1
11

MONITOR
1562
634
1716
679
% emissions PF egg
precision emissions-share-pf-egg 2
17
1
11

MONITOR
1562
587
1716
632
% emissions PF dairy
precision emissions-share-pf-dairy 2
17
1
11

MONITOR
1562
540
1716
585
% emissions CM meat
precision emissions-share-cm 2
17
1
11

PLOT
1205
270
1467
410
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
"Beef" 1.0 0 -5298144 true "" "plot av-emissions-kg-beef"
"Lamb" 1.0 0 -10603201 true "" "plot av-emissions-kg-lamb"
"Pork" 1.0 0 -13360827 true "" "plot av-emissions-kg-pork"
"Chicken" 1.0 0 -13210332 true "" "plot av-emissions-kg-chicken"

SLIDER
1205
201
1404
234
carbon-tax-per-tonne
carbon-tax-per-tonne
0
5000
1000.0
10
1
NOK
HORIZONTAL

TEXTBOX
1205
185
1491
213
Carbon tax rate (NOK/tonne CO2-eq) and start year
11
0.0
1

SLIDER
1406
201
1605
234
carbon-tax-start-yr
carbon-tax-start-yr
2013
2050
2025.0
1
1
NIL
HORIZONTAL

PLOT
1723
267
1922
387
Total farm income
NIL
bn/NOK
1.0
37.0
0.0
86.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-farm-income / 1000000000"

PLOT
1924
267
2123
387
Average farm income
NIL
1000/NOK
1.0
37.0
0.0
2500.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (total-farm-income / total-farms) / 1000"

PLOT
2629
23
2930
143
Surviving specialist cattle farms
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
"default" 1.0 0 -16777216 true "" "plot (specialist-cattle-farms / init-num-specialist-cattle-farms) * 100"

PLOT
2629
145
2930
265
Surviving specialist sheep farms
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
"default" 1.0 0 -16777216 true "" "plot (specialist-sheep-farms / init-num-specialist-sheep-farms) * 100"

PLOT
2629
267
2930
387
Surviving specialist pig farms
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
"default" 1.0 0 -16777216 true "" "plot (specialist-pig-farms / init-num-specialist-pig-farms) * 100"

PLOT
2629
389
2930
509
Surviving specialist broiler farms
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
"default" 1.0 0 -16777216 true "" "plot (specialist-broiler-farms / init-num-specialist-broiler-farms) * 100"

PLOT
2629
511
2930
631
Surviving specialist laying hen farms
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
"default" 1.0 0 -16777216 true "" "plot (specialist-laying-hen-farms / init-num-specialist-laying-hen-farms) * 100"

TEXTBOX
2628
10
2778
28
Farm survival by type
11
0.0
1

PLOT
2629
633
2930
753
Surviving specialist fodder/silage farms
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
"default" 1.0 0 -16777216 true "" "plot (specialist-fodder-silage-farms / init-num-specialist-fodder-silage-farms) * 100"

PLOT
2629
755
2930
875
Surviving specialist arable/horticulture farms
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
"default" 1.0 0 -16777216 true "" "plot (specialist-arable-horticulture-farms / init-num-specialist-arable-horticulture-farms) * 100"

PLOT
2629
877
2930
997
Surviving combined cattle-grain farms
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
"default" 1.0 0 -16777216 true "" "plot (combined-cattle-grain-farms / init-num-combined-cattle-grain-farms) * 100"

PLOT
2629
999
2930
1119
Surviving combined cattle-sheep farms
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
"default" 1.0 0 -16777216 true "" "plot (combined-cattle-sheep-farms / init-num-combined-cattle-sheep-farms) * 100"

PLOT
2629
1121
2930
1241
Surviving other mixed farms
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
"default" 1.0 0 -16777216 true "" "plot (other-mixed-farms / init-num-other-mixed-farms) * 100"

PLOT
2629
1243
2930
1363
Surviving farms with no activity
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
"default" 1.0 0 -16777216 true "" "plot (no-activity-farms / init-num-no-activity-farms) * 100"

SWITCH
193
707
411
740
highlight-inactive-farms
highlight-inactive-farms
1
1
-1000

SLIDER
744
332
944
365
cease-farming-prob
cease-farming-prob
0
1
0.25
0.05
1
NIL
HORIZONTAL

CHOOSER
1482
238
1682
283
emissions-tax-coverage
emissions-tax-coverage
"Agriculture" "Agriculture & energy" "Agriculture, energy & LULUCF"
1

MONITOR
1616
1000
1815
1045
CM lamb profit margin (NOK/t)
profit-margin-cm-lamb
0
1
11

MONITOR
1616
1122
1815
1167
CM pork profit margin (NOK/t)
profit-margin-cm-pork
0
1
11

MONITOR
1616
1244
1815
1289
CM chicken profit margin (NOK/t)
profit-margin-cm-chicken
0
1
11

MONITOR
1616
1366
1815
1411
PF egg profit margin (NOK/t)
profit-margin-pf-eggs
0
1
11

MONITOR
1616
1488
1815
1533
PF milk profit margin (NOK/t)
profit-margin-pf-raw-milk
0
1
11

MONITOR
1616
1291
1815
1336
CM chicken profit margin (%)
profit-margin-perc-cm-chicken
2
1
11

MONITOR
1616
1169
1815
1214
CM pork profit margin (%)
profit-margin-perc-cm-pork
2
1
11

MONITOR
1616
1047
1815
1092
CM lamb profit margin (%)
profit-margin-perc-cm-lamb
2
1
11

MONITOR
1616
925
1815
970
CM beef profit margin (%)
profit-margin-perc-cm-beef
2
1
11

MONITOR
1616
1413
1815
1458
PF egg profit margin (%)
profit-margin-perc-pf-eggs
2
1
11

MONITOR
1616
1535
1815
1580
PF milk profit margin (%)
profit-margin-perc-pf-raw-milk
2
1
11

MONITOR
1616
878
1815
923
CM beef profit margin (NOK/t)
profit-margin-cm-beef
0
1
11

SLIDER
744
367
944
400
cf-required-profit-margin
cf-required-profit-margin
0
30
10.0
1
1
%
HORIZONTAL

TEXTBOX
8
1900
714
1947
NO_Protein_ABM. Copyright (C) 2023. The James Hutton Institute.\nThis program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions. For more information on this and the (lack of) warranty, see the LICENCE section in the Info tab.
11
0.0
1

MONITOR
413
707
537
752
Seed
starting-seed
0
1
11

SWITCH
950
693
1198
726
play-step-sound?
play-step-sound?
1
1
-1000

SWITCH
950
658
1198
691
play-end-sound?
play-end-sound?
0
1
-1000

@#$#@#$#@
## CREDITS AND REFERENCES

Authors: Nick Roxburgh (nick.roxburgh@hutton.ac.uk), Gary Polhill (gary.polhill@hutton.ac.uk), Rob Burton (rob.burton@ruralis.no), Klaus Mittenzwei (klaus.mittenzwei@ruralis.no)

Data references:
- Animalia 2021. Kjøttets Tilstand 2021: Status i norsk kjøtt- og eggproduksjon
Source URL: https://www.animalia.no/globalassets/kjottets-tilstand/kt21-web-endelig.pdf. Data acquired: 17 August 2022
- Boyd, J.H. (2020) Biomanufacturing Costs in Cities around the Globe. Genetic Engineering & Biotechnology News. Vol 40. No.6. https://www.genengnews.com/topics/bioprocessing/biomanufacturing-costs-in-cities-around-the-globe/. Accessed 4 September 2022.
- Conroy, S.B., Drennan, M.J., McGee, M., Keane, M.G., Kenny, D.A. and Berry, D.P., 2010. Predicting beef carcass meat, fat and bone proportions from carcass conformation and fat scores or hindquarter dissection. Animal, 4(2), pp.234-241.
- Diplom-Is (2022). Om oss. https://www.diplom-is.no/om-oss/selskapet. Accessed 16 September 2022.
- Europe Economics (2017). The Brewers of Europe Cost comparability study Final report. https://brewersofeurope.org/uploads/mycms-files/documents/publications/2017/Cost%20Comparability%20Study%20-%20Europe%20Economics%20February%202017.pdf. Accessed 4 September 2022.
- Eurostat (2013). File:Cow's milk apparent yield, 2011.png. https://ec.europa.eu/eurostat/statistics-explained/index.php?title=File:Cow%27s_milk_apparent_yield,_2011.png. Accessed 4 September 2022.
- Eurostat/GISCO (2014). Ports, 2013-TransportNetworks-Dataset. https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/transport-networks. Data accessed: 12 April 2022.
- FAOSTAT (2022). Producer Price (LCU/tonne). Date accessed: 2 November 2022
URL: https://www.fao.org/faostat/en/#search/producer%20price
- Geonorge (2022a). File names: Basisdata_0000_Norge_25833_Kommuner_GML.gml. Data acquired: 16 April 2022. https://kartkatalog.geonorge.no/metadata/administrative-enheter-kommuner/041f1e6e-bdbc-4091-b48f-8a5990f3cc5b. Source title: Administrative enheter kommuner. Accessed 29 March 2022.
- Geonorge (2022b). Data acquired: 9 August 2022. Source URL: https://kartkatalog.geonorge.no/metadata/administrative-enheter-fylker/6093c8a8-fa80-11e6-bc64-92361f002671. Source title: Administrative enheter fylker. 
- Gustavsen, G.W. and Mittenzwei, K., 2022. Potential demand for synthetic meat. Proceedings in Food System Dynamics, pp.44-52.
- Helsedirektoratet (2021). Utviklingen I norsk kosthold: 2021. 
https://www.helsedirektoratet.no/rapporter/utviklingen-i-norsk-kosthold. Accessed 26 October 2022.
- Hennig Olsen (2022). Hennig Olsen: Kremen av iskrem. https://www.hennig-olsen.no/. Accessed 16 September 2022.
- Kavli (2022). About Us. https://www.kavli.com. Accessed 16 September 2022.
- Kongsro, J., Røe, M., Kvaal, K., Aastveit, A.H. and Egelandsdal, B., 2009. Prediction of fat, muscle and value in Norwegian lamb carcasses using EUROP classification, carcass shape and length measurements, visible light reflectance and computer tomography (CT). Meat Science, 81(1), pp.102-107.
- Milczarek, A., Pachnik, M., Osek, M. and Świnarska, R., 2022. Rearing Performance and Carcass Composition of Broiler Chickens Fed Rations Containing Guar Meal at Graded Levels. Agriculture, 12(9), p.1385.
- Mittenzwei, K. and Prestvik, A.S. (2022). Klimagassutslipp fra norsk jordbruk fordelt på areal, dyr og matproduksjon. PLATON Rapport - analyse 5/2022. Available from: https://www.platonklima.no/wp-content/uploads/2022/02/Rapport-analyse-5-2022-Klimagassutslipp-fra-jordbruk-1.pdf. Accessed: 12 January 2022.
- Norwegian Agriculture Authority (2021). Felles datakatalog. https://data.norge.no/datasets?orgPath=%2FSTAT%2F972417874%2F981544315&q=produksjonstilskudd. Accessed 18 February 2021.
- Pulkrábek, J., Pavlík, J., Valis, L. and Vítek, M., 2006. Pig carcass quality in relation to carcass lean meat proportion. Czech Journal of Animal Science, 51(1), p.18.
- Q-Meieriene (2022). Om Q-Meieriene. https://www.q-meieriene.no/. Accessed 16 September 2022.
- Rogelj, J., Popp, A., Calvin, K.V., Luderer, G., Emmerling, J., Gernaat, D., Fujimori, S., Strefler, J., Hasegawa, T., Marangoni, G. and Krey, V., 2018. Scenarios towards limiting global mean temperature increase below 1.5 C. Nature Climate Change, 8(4), pp.325-332.
- Sinke, P., Swartz, E., Sanctorum, H., van der Giesen, C. and Odegard, I., 2023. Ex‐ante life cycle assessment of commercial‐scale cultivated meat production in 2030. The International Journal of Life Cycle Assessment. 28: 234-254. DOI: 10.1007/s11367-022-02128-8
- Statistisk Sentralbyrå, 2022a https://www.ssb.no/en/statbank/table/01222/tableViewLayout1/. Source title: 01222: Population and changes during the quarter, by contents, region and quarter. Data acquired: 14 August 2022
- Statistisk Sentralbyrå, 2022b. Data acquired: 14 August 2022. Source URL: https://www.ssb.no/en/statbank/table/13600/tableViewLayout1/. Source title: 13600: Population projections 1 January, by region, year and contents
- Statistisk Sentralbyrå (2022c) Source title: 04609: Total production (1 000 tons), by contents, region and year. Source URL: https://www.ssb.no/en/statbank/table/04609/tableViewLayout1/. Data acquired: 5 August 2022
- Statistisk Sentralbyrå (2022d) Source title: 05772: Yield of agricultural crops (1 000 tonnes), by contents, region and year. Source URL: https://www.ssb.no/en/statbank/table/05772/tableViewLayout1/. Date acquired: 10 August 2022
- Statistisk Sentralbyrå (2022e) Source title: 10508: Yield (tonnes), by horticultural crop, contents and year. Source URL: https://www.ssb.no/en/statbank/table/10508/tableViewLayout1/. Data acquired: 10 August 2022
- Statistisk Sentralbyrå (2022f) Source title: 11506: Agricultural area (decares), by contents, region, crop and year. Source URL: https://www.ssb.no/en/statbank/table/11506/tableViewLayout1/. Data acquired: 5 August 2022
- Statistisk Sentralbyrå (2022g). 03551: Public meat inspection. Carcasses approved for human consumption (tonnes), by contents, region, carcasses approved and year. Data acquired: 11 August 2022. Source URL: https://www.ssb.no/en/statbank/table/03551/tableViewLayout1/
- Statistisk Sentralbyrå (2022h). 11507: Domestic animals, by contents, region, domestic animals of various kinds and year. Source URL: https://www.ssb.no/en/statbank/table/11507/tableViewLayout1/. Data acquired: 15 August 2022
- Steer, M. (2015). A comparison of land, water and energy use between conventional and yeast-derived dairy products: An initial analysis. Report prepared for Perfect Day. Accessed 4 October 2022.
- Svindland, M., Monios, J., Hjelle, H. (2019). Port rationalization and the evolution of regional port systems: the case of Norway. Maritime Policy & Management. 46 (5): 613-629. DOI: 10.1080/03088839.2019.1574988
- Synnove (2022). Synnøve i dag. https://www.synnove.no/synnøve-i-dag/. Accessed 16 September 2022.
- TINE (2022). Om TINE Meieriene. https://www.tine.no/om-tine/meieriene. Accessed 16 September 2022.
- Tingvollost (2022). Om Tingvollost. https://www.tingvollost.no/om-tingvollost1/om-tingvollost. Accessed 16 September 2022.
- US Department of Agriculture (2019a). FoodData Central. Butter, without salt. https://fdc.nal.usda.gov/fdc-app.html#/food-details/173430/nutrients. Accessed 14 November 2022.
- US Department of Agriculture (2019b). FoodData Central. Cheese, cheddar. https://fdc.nal.usda.gov/fdc-app.html#/food-details/328637/nutrients. Accessed 14 November 2022.
- US Department of Agriculture (2019c). FoodData Central. Eggs, Grade A, Large, egg whole. https://fdc.nal.usda.gov/fdc-app.html#/food-details/748967/nutrients. Accessed 14 November 2022.
- US Department of Agriculture (2019d). FoodData Central. Milk, fluid, 1% fat, without added vitamin A and vitamin D. https://fdc.nal.usda.gov/fdc-app.html#/food-details/173441/nutrients. Accessed 14 November 2022.
- US Department of Agriculture (2019e). FoodData Central. Yogurt, plain, whole milk. https://fdc.nal.usda.gov/fdc-app.html#/food-details/2259793/nutrients. Accessed 14 November 2022.
- Vergeer, R., Sinke, P., and Odegard, I. 2021. TEA of cultivated meat: Future projections of different scenarios – corrigendum. Delft, CE Delft. https://cedelft.eu/publications/tea-of-cultivated-meat/. Accessed 4 September 2022.
- Wikipedia (2022a) Norway–Sweden border. https://en.wikipedia.org/wiki/Norway–Sweden_border. Accessed 22 August 2022.
- Wikipedia (2022b) Finland–Norway border. https://en.wikipedia.org/wiki/Finland–Norway_border. Accessed 22 August 2022.
- Wikipedia (2022c) Norway–Russia border. https://en.wikipedia.org/wiki/Norway–Russia_border. Accessed 22 August 2022.
- WSP (2021). ISO-Conformant Report: Comparative Life Cycle Assessment of Perfect Day Whey Protein Production to Dairy Protein. Report No. 1. Portland: WSP.


## LICENCE
```text
                        GNU GENERAL PUBLIC LICENSE
                           Version 3, 29 June 2007
    
     Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
     Everyone is permitted to copy and distribute verbatim copies
     of this license document, but changing it is not allowed.
    
                                Preamble
    
      The GNU General Public License is a free, copyleft license for
    software and other kinds of works.
    
      The licenses for most software and other practical works are designed
    to take away your freedom to share and change the works.  By contrast,
    the GNU General Public License is intended to guarantee your freedom to
    share and change all versions of a program--to make sure it remains free
    software for all its users.  We, the Free Software Foundation, use the
    GNU General Public License for most of our software; it applies also to
    any other work released this way by its authors.  You can apply it to
    your programs, too.
    
      When we speak of free software, we are referring to freedom, not
    price.  Our General Public Licenses are designed to make sure that you
    have the freedom to distribute copies of free software (and charge for
    them if you wish), that you receive source code or can get it if you
    want it, that you can change the software or use pieces of it in new
    free programs, and that you know you can do these things.
    
      To protect your rights, we need to prevent others from denying you
    these rights or asking you to surrender the rights.  Therefore, you have
    certain responsibilities if you distribute copies of the software, or if
    you modify it: responsibilities to respect the freedom of others.
    
      For example, if you distribute copies of such a program, whether
    gratis or for a fee, you must pass on to the recipients the same
    freedoms that you received.  You must make sure that they, too, receive
    or can get the source code.  And you must show them these terms so they
    know their rights.
    
      Developers that use the GNU GPL protect your rights with two steps:
    (1) assert copyright on the software, and (2) offer you this License
    giving you legal permission to copy, distribute and/or modify it.
    
      For the developers' and authors' protection, the GPL clearly explains
    that there is no warranty for this free software.  For both users' and
    authors' sake, the GPL requires that modified versions be marked as
    changed, so that their problems will not be attributed erroneously to
    authors of previous versions.
    
      Some devices are designed to deny users access to install or run
    modified versions of the software inside them, although the manufacturer
    can do so.  This is fundamentally incompatible with the aim of
    protecting users' freedom to change the software.  The systematic
    pattern of such abuse occurs in the area of products for individuals to
    use, which is precisely where it is most unacceptable.  Therefore, we
    have designed this version of the GPL to prohibit the practice for those
    products.  If such problems arise substantially in other domains, we
    stand ready to extend this provision to those domains in future versions
    of the GPL, as needed to protect the freedom of users.
    
      Finally, every program is threatened constantly by software patents.
    States should not allow patents to restrict development and use of
    software on general-purpose computers, but in those that do, we wish to
    avoid the special danger that patents applied to a free program could
    make it effectively proprietary.  To prevent this, the GPL assures that
    patents cannot be used to render the program non-free.
    
      The precise terms and conditions for copying, distribution and
    modification follow.
    
                           TERMS AND CONDITIONS
    
      0. Definitions.
    
      "This License" refers to version 3 of the GNU General Public License.
    
      "Copyright" also means copyright-like laws that apply to other kinds of
    works, such as semiconductor masks.
    
      "The Program" refers to any copyrightable work licensed under this
    License.  Each licensee is addressed as "you".  "Licensees" and
    "recipients" may be individuals or organizations.
    
      To "modify" a work means to copy from or adapt all or part of the work
    in a fashion requiring copyright permission, other than the making of an
    exact copy.  The resulting work is called a "modified version" of the
    earlier work or a work "based on" the earlier work.
    
      A "covered work" means either the unmodified Program or a work based
    on the Program.
    
      To "propagate" a work means to do anything with it that, without
    permission, would make you directly or secondarily liable for
    infringement under applicable copyright law, except executing it on a
    computer or modifying a private copy.  Propagation includes copying,
    distribution (with or without modification), making available to the
    public, and in some countries other activities as well.
    
      To "convey" a work means any kind of propagation that enables other
    parties to make or receive copies.  Mere interaction with a user through
    a computer network, with no transfer of a copy, is not conveying.
    
      An interactive user interface displays "Appropriate Legal Notices"
    to the extent that it includes a convenient and prominently visible
    feature that (1) displays an appropriate copyright notice, and (2)
    tells the user that there is no warranty for the work (except to the
    extent that warranties are provided), that licensees may convey the
    work under this License, and how to view a copy of this License.  If
    the interface presents a list of user commands or options, such as a
    menu, a prominent item in the list meets this criterion.
    
      1. Source Code.
    
      The "source code" for a work means the preferred form of the work
    for making modifications to it.  "Object code" means any non-source
    form of a work.
    
      A "Standard Interface" means an interface that either is an official
    standard defined by a recognized standards body, or, in the case of
    interfaces specified for a particular programming language, one that
    is widely used among developers working in that language.
    
      The "System Libraries" of an executable work include anything, other
    than the work as a whole, that (a) is included in the normal form of
    packaging a Major Component, but which is not part of that Major
    Component, and (b) serves only to enable use of the work with that
    Major Component, or to implement a Standard Interface for which an
    implementation is available to the public in source code form.  A
    "Major Component", in this context, means a major essential component
    (kernel, window system, and so on) of the specific operating system
    (if any) on which the executable work runs, or a compiler used to
    produce the work, or an object code interpreter used to run it.
        
      The "Corresponding Source" for a work in object code form means all
    the source code needed to generate, install, and (for an executable
    work) run the object code and to modify the work, including scripts to
    control those activities.  However, it does not include the work's
    System Libraries, or general-purpose tools or generally available free
    programs which are used unmodified in performing those activities but
    which are not part of the work.  For example, Corresponding Source
    includes interface definition files associated with source files for
    the work, and the source code for shared libraries and dynamically
    linked subprograms that the work is specifically designed to require,
    such as by intimate data communication or control flow between those
    subprograms and other parts of the work.
    
      The Corresponding Source need not include anything that users
    can regenerate automatically from other parts of the Corresponding
    Source.
    
      The Corresponding Source for a work in source code form is that
    same work.
    
      2. Basic Permissions.
    
      All rights granted under this License are granted for the term of
    copyright on the Program, and are irrevocable provided the stated
    conditions are met.  This License explicitly affirms your unlimited
    permission to run the unmodified Program.  The output from running a
    covered work is covered by this License only if the output, given its
    content, constitutes a covered work.  This License acknowledges your
    rights of fair use or other equivalent, as provided by copyright law.
    
      You may make, run and propagate covered works that you do not
    convey, without conditions so long as your license otherwise remains
    in force.  You may convey covered works to others for the sole purpose
    of having them make modifications exclusively for you, or provide you
    with facilities for running those works, provided that you comply with
    the terms of this License in conveying all material for which you do
    not control copyright.  Those thus making or running the covered works
    for you must do so exclusively on your behalf, under your direction
    and control, on terms that prohibit them from making any copies of
    your copyrighted material outside their relationship with you.
    
      Conveying under any other circumstances is permitted solely under
    the conditions stated below.  Sublicensing is not allowed; section 10
    makes it unnecessary.
    
      3. Protecting Users' Legal Rights From Anti-Circumvention Law.
    
      No covered work shall be deemed part of an effective technological
    measure under any applicable law fulfilling obligations under article
    11 of the WIPO copyright treaty adopted on 20 December 1996, or
    similar laws prohibiting or restricting circumvention of such
    measures.
    
      When you convey a covered work, you waive any legal power to forbid
    circumvention of technological measures to the extent such circumvention
    is effected by exercising rights under this License with respect to
    the covered work, and you disclaim any intention to limit operation or
    modification of the work as a means of enforcing, against the work's
    users, your or third parties' legal rights to forbid circumvention of
    technological measures.
    
      4. Conveying Verbatim Copies.
    
      You may convey verbatim copies of the Program's source code as you
    receive it, in any medium, provided that you conspicuously and
    appropriately publish on each copy an appropriate copyright notice;
    keep intact all notices stating that this License and any
    non-permissive terms added in accord with section 7 apply to the code;
    keep intact all notices of the absence of any warranty; and give all
    recipients a copy of this License along with the Program.
    
      You may charge any price or no price for each copy that you convey,
    and you may offer support or warranty protection for a fee.
    
      5. Conveying Modified Source Versions.
    
      You may convey a work based on the Program, or the modifications to
    produce it from the Program, in the form of source code under the
    terms of section 4, provided that you also meet all of these conditions:
    
        a) The work must carry prominent notices stating that you modified
        it, and giving a relevant date.
    
        b) The work must carry prominent notices stating that it is
        released under this License and any conditions added under section
        7.  This requirement modifies the requirement in section 4 to
        "keep intact all notices".
    
        c) You must license the entire work, as a whole, under this
        License to anyone who comes into possession of a copy.  This
        License will therefore apply, along with any applicable section 7
        additional terms, to the whole of the work, and all its parts,
        regardless of how they are packaged.  This License gives no
        permission to license the work in any other way, but it does not
        invalidate such permission if you have separately received it.
    
        d) If the work has interactive user interfaces, each must display
        Appropriate Legal Notices; however, if the Program has interactive
        interfaces that do not display Appropriate Legal Notices, your
        work need not make them do so.
    
      A compilation of a covered work with other separate and independent
    works, which are not by their nature extensions of the covered work,
    and which are not combined with it such as to form a larger program,
    in or on a volume of a storage or distribution medium, is called an
    "aggregate" if the compilation and its resulting copyright are not
    used to limit the access or legal rights of the compilation's users
    beyond what the individual works permit.  Inclusion of a covered work
    in an aggregate does not cause this License to apply to the other
    parts of the aggregate.
    
      6. Conveying Non-Source Forms.
    
      You may convey a covered work in object code form under the terms
    of sections 4 and 5, provided that you also convey the
    machine-readable Corresponding Source under the terms of this License,
    in one of these ways:
    
        a) Convey the object code in, or embodied in, a physical product
        (including a physical distribution medium), accompanied by the
        Corresponding Source fixed on a durable physical medium
        customarily used for software interchange.
    
        b) Convey the object code in, or embodied in, a physical product
        (including a physical distribution medium), accompanied by a
        written offer, valid for at least three years and valid for as
        long as you offer spare parts or customer support for that product
        model, to give anyone who possesses the object code either (1) a
        copy of the Corresponding Source for all the software in the
        product that is covered by this License, on a durable physical
        medium customarily used for software interchange, for a price no
        more than your reasonable cost of physically performing this
        conveying of source, or (2) access to copy the
        Corresponding Source from a network server at no charge.
    
        c) Convey individual copies of the object code with a copy of the
        written offer to provide the Corresponding Source.  This
        alternative is allowed only occasionally and noncommercially, and
        only if you received the object code with such an offer, in accord
        with subsection 6b.
    
        d) Convey the object code by offering access from a designated
        place (gratis or for a charge), and offer equivalent access to the
        Corresponding Source in the same way through the same place at no
        further charge.  You need not require recipients to copy the
        Corresponding Source along with the object code.  If the place to
        copy the object code is a network server, the Corresponding Source
        may be on a different server (operated by you or a third party)
        that supports equivalent copying facilities, provided you maintain
        clear directions next to the object code saying where to find the
        Corresponding Source.  Regardless of what server hosts the
        Corresponding Source, you remain obligated to ensure that it is
        available for as long as needed to satisfy these requirements.
    
        e) Convey the object code using peer-to-peer transmission, provided
        you inform other peers where the object code and Corresponding
        Source of the work are being offered to the general public at no
        charge under subsection 6d.
    
      A separable portion of the object code, whose source code is excluded
    from the Corresponding Source as a System Library, need not be
    included in conveying the object code work.
    
      A "User Product" is either (1) a "consumer product", which means any
    tangible personal property which is normally used for personal, family,
    or household purposes, or (2) anything designed or sold for incorporation
    into a dwelling.  In determining whether a product is a consumer product,
    doubtful cases shall be resolved in favor of coverage.  For a particular
    product received by a particular user, "normally used" refers to a
    typical or common use of that class of product, regardless of the status
    of the particular user or of the way in which the particular user
    actually uses, or expects or is expected to use, the product.  A product
    is a consumer product regardless of whether the product has substantial
    commercial, industrial or non-consumer uses, unless such uses represent
    the only significant mode of use of the product.
    
      "Installation Information" for a User Product means any methods,
    procedures, authorization keys, or other information required to install
    and execute modified versions of a covered work in that User Product from
    a modified version of its Corresponding Source.  The information must
    suffice to ensure that the continued functioning of the modified object
    code is in no case prevented or interfered with solely because
    modification has been made.
    
      If you convey an object code work under this section in, or with, or
    specifically for use in, a User Product, and the conveying occurs as
    part of a transaction in which the right of possession and use of the
    User Product is transferred to the recipient in perpetuity or for a
    fixed term (regardless of how the transaction is characterized), the
    Corresponding Source conveyed under this section must be accompanied
    by the Installation Information.  But this requirement does not apply
    if neither you nor any third party retains the ability to install
    modified object code on the User Product (for example, the work has
    been installed in ROM).
    
      The requirement to provide Installation Information does not include a
    requirement to continue to provide support service, warranty, or updates
    for a work that has been modified or installed by the recipient, or for
    the User Product in which it has been modified or installed.  Access to a
    network may be denied when the modification itself materially and
    adversely affects the operation of the network or violates the rules and
    protocols for communication across the network.
    
      Corresponding Source conveyed, and Installation Information provided,
    in accord with this section must be in a format that is publicly
    documented (and with an implementation available to the public in
    source code form), and must require no special password or key for
    unpacking, reading or copying.
    
      7. Additional Terms.
    
      "Additional permissions" are terms that supplement the terms of this
    License by making exceptions from one or more of its conditions.
    Additional permissions that are applicable to the entire Program shall
    be treated as though they were included in this License, to the extent
    that they are valid under applicable law.  If additional permissions
    apply only to part of the Program, that part may be used separately
    under those permissions, but the entire Program remains governed by
    this License without regard to the additional permissions.
    
      When you convey a copy of a covered work, you may at your option
    remove any additional permissions from that copy, or from any part of
    it.  (Additional permissions may be written to require their own
    removal in certain cases when you modify the work.)  You may place
    additional permissions on material, added by you to a covered work,
    for which you have or can give appropriate copyright permission.
    
      Notwithstanding any other provision of this License, for material you
    add to a covered work, you may (if authorized by the copyright holders of
    that material) supplement the terms of this License with terms:
    
        a) Disclaiming warranty or limiting liability differently from the
        terms of sections 15 and 16 of this License; or
    
        b) Requiring preservation of specified reasonable legal notices or
        author attributions in that material or in the Appropriate Legal
        Notices displayed by works containing it; or
    
        c) Prohibiting misrepresentation of the origin of that material, or
        requiring that modified versions of such material be marked in
        reasonable ways as different from the original version; or
    
        d) Limiting the use for publicity purposes of names of licensors or
        authors of the material; or
    
        e) Declining to grant rights under trademark law for use of some
        trade names, trademarks, or service marks; or
    
        f) Requiring indemnification of licensors and authors of that
        material by anyone who conveys the material (or modified versions of
        it) with contractual assumptions of liability to the recipient, for
        any liability that these contractual assumptions directly impose on
        those licensors and authors.
    
      All other non-permissive additional terms are considered "further
    restrictions" within the meaning of section 10.  If the Program as you
    received it, or any part of it, contains a notice stating that it is
    governed by this License along with a term that is a further
    restriction, you may remove that term.  If a license document contains
    a further restriction but permits relicensing or conveying under this
    License, you may add to a covered work material governed by the terms
    of that license document, provided that the further restriction does
    not survive such relicensing or conveying.
    
      If you add terms to a covered work in accord with this section, you
    must place, in the relevant source files, a statement of the
    additional terms that apply to those files, or a notice indicating
    where to find the applicable terms.
    
      Additional terms, permissive or non-permissive, may be stated in the
    form of a separately written license, or stated as exceptions;
    the above requirements apply either way.
    
      8. Termination.
    
      You may not propagate or modify a covered work except as expressly
    provided under this License.  Any attempt otherwise to propagate or
    modify it is void, and will automatically terminate your rights under
    this License (including any patent licenses granted under the third
    paragraph of section 11).
    
      However, if you cease all violation of this License, then your
    license from a particular copyright holder is reinstated (a)
    provisionally, unless and until the copyright holder explicitly and
    finally terminates your license, and (b) permanently, if the copyright
    holder fails to notify you of the violation by some reasonable means
    prior to 60 days after the cessation.
    
      Moreover, your license from a particular copyright holder is
    reinstated permanently if the copyright holder notifies you of the
    violation by some reasonable means, this is the first time you have
    received notice of violation of this License (for any work) from that
    copyright holder, and you cure the violation prior to 30 days after
    your receipt of the notice.
    
      Termination of your rights under this section does not terminate the
    licenses of parties who have received copies or rights from you under
    this License.  If your rights have been terminated and not permanently
    reinstated, you do not qualify to receive new licenses for the same
    material under section 10.
    
      9. Acceptance Not Required for Having Copies.
    
      You are not required to accept this License in order to receive or
    run a copy of the Program.  Ancillary propagation of a covered work
    occurring solely as a consequence of using peer-to-peer transmission
    to receive a copy likewise does not require acceptance.  However,
    nothing other than this License grants you permission to propagate or
    modify any covered work.  These actions infringe copyright if you do
    not accept this License.  Therefore, by modifying or propagating a
    covered work, you indicate your acceptance of this License to do so.
    
      10. Automatic Licensing of Downstream Recipients.
    
      Each time you convey a covered work, the recipient automatically
    receives a license from the original licensors, to run, modify and
    propagate that work, subject to this License.  You are not responsible
    for enforcing compliance by third parties with this License.
    
      An "entity transaction" is a transaction transferring control of an
    organization, or substantially all assets of one, or subdividing an
    organization, or merging organizations.  If propagation of a covered
    work results from an entity transaction, each party to that
    transaction who receives a copy of the work also receives whatever
    licenses to the work the party's predecessor in interest had or could
    give under the previous paragraph, plus a right to possession of the
    Corresponding Source of the work from the predecessor in interest, if
    the predecessor has it or can get it with reasonable efforts.
    
      You may not impose any further restrictions on the exercise of the
    rights granted or affirmed under this License.  For example, you may
    not impose a license fee, royalty, or other charge for exercise of
    rights granted under this License, and you may not initiate litigation
    (including a cross-claim or counterclaim in a lawsuit) alleging that
    any patent claim is infringed by making, using, selling, offering for
    sale, or importing the Program or any portion of it.
    
      11. Patents.
    
      A "contributor" is a copyright holder who authorizes use under this
    License of the Program or a work on which the Program is based.  The
    work thus licensed is called the contributor's "contributor version".
    
      A contributor's "essential patent claims" are all patent claims
    owned or controlled by the contributor, whether already acquired or
    hereafter acquired, that would be infringed by some manner, permitted
    by this License, of making, using, or selling its contributor version,
    but do not include claims that would be infringed only as a
    consequence of further modification of the contributor version.  For
    purposes of this definition, "control" includes the right to grant
    patent sublicenses in a manner consistent with the requirements of
    this License.
    
      Each contributor grants you a non-exclusive, worldwide, royalty-free
    patent license under the contributor's essential patent claims, to
    make, use, sell, offer for sale, import and otherwise run, modify and
    propagate the contents of its contributor version.
    
      In the following three paragraphs, a "patent license" is any express
    agreement or commitment, however denominated, not to enforce a patent
    (such as an express permission to practice a patent or covenant not to
    sue for patent infringement).  To "grant" such a patent license to a
    party means to make such an agreement or commitment not to enforce a
    patent against the party.
    
      If you convey a covered work, knowingly relying on a patent license,
    and the Corresponding Source of the work is not available for anyone
    to copy, free of charge and under the terms of this License, through a
    publicly available network server or other readily accessible means,
    then you must either (1) cause the Corresponding Source to be so
    available, or (2) arrange to deprive yourself of the benefit of the
    patent license for this particular work, or (3) arrange, in a manner
    consistent with the requirements of this License, to extend the patent
    license to downstream recipients.  "Knowingly relying" means you have
    actual knowledge that, but for the patent license, your conveying the
    covered work in a country, or your recipient's use of the covered work
    in a country, would infringe one or more identifiable patents in that
    country that you have reason to believe are valid.
    
      If, pursuant to or in connection with a single transaction or
    arrangement, you convey, or propagate by procuring conveyance of, a
    covered work, and grant a patent license to some of the parties
    receiving the covered work authorizing them to use, propagate, modify
    or convey a specific copy of the covered work, then the patent license
    you grant is automatically extended to all recipients of the covered
    work and works based on it.
    
      A patent license is "discriminatory" if it does not include within
    the scope of its coverage, prohibits the exercise of, or is
    conditioned on the non-exercise of one or more of the rights that are
    specifically granted under this License.  You may not convey a covered
    work if you are a party to an arrangement with a third party that is
    in the business of distributing software, under which you make payment
    to the third party based on the extent of your activity of conveying
    the work, and under which the third party grants, to any of the
    parties who would receive the covered work from you, a discriminatory
    patent license (a) in connection with copies of the covered work
    conveyed by you (or copies made from those copies), or (b) primarily
    for and in connection with specific products or compilations that
    contain the covered work, unless you entered into that arrangement,
    or that patent license was granted, prior to 28 March 2007.
    
      Nothing in this License shall be construed as excluding or limiting
    any implied license or other defenses to infringement that may
    otherwise be available to you under applicable patent law.
    
      12. No Surrender of Others' Freedom.
    
      If conditions are imposed on you (whether by court order, agreement or
    otherwise) that contradict the conditions of this License, they do not
    excuse you from the conditions of this License.  If you cannot convey a
    covered work so as to satisfy simultaneously your obligations under this
    License and any other pertinent obligations, then as a consequence you may
    not convey it at all.  For example, if you agree to terms that obligate you
    to collect a royalty for further conveying from those to whom you convey
    the Program, the only way you could satisfy both those terms and this
    License would be to refrain entirely from conveying the Program.
    
      13. Use with the GNU Affero General Public License.
    
      Notwithstanding any other provision of this License, you have
    permission to link or combine any covered work with a work licensed
    under version 3 of the GNU Affero General Public License into a single
    combined work, and to convey the resulting work.  The terms of this
    License will continue to apply to the part which is the covered work,
    but the special requirements of the GNU Affero General Public License,
    section 13, concerning interaction through a network will apply to the
    combination as such.
    
      14. Revised Versions of this License.
    
      The Free Software Foundation may publish revised and/or new versions of
    the GNU General Public License from time to time.  Such new versions will
    be similar in spirit to the present version, but may differ in detail to
    address new problems or concerns.
    
      Each version is given a distinguishing version number.  If the
    Program specifies that a certain numbered version of the GNU General
    Public License "or any later version" applies to it, you have the
    option of following the terms and conditions either of that numbered
    version or of any later version published by the Free Software
    Foundation.  If the Program does not specify a version number of the
    GNU General Public License, you may choose any version ever published
    by the Free Software Foundation.
    
      If the Program specifies that a proxy can decide which future
    versions of the GNU General Public License can be used, that proxy's
    public statement of acceptance of a version permanently authorizes you
    to choose that version for the Program.
    
      Later license versions may give you additional or different
    permissions.  However, no additional obligations are imposed on any
    author or copyright holder as a result of your choosing to follow a
    later version.
    
      15. Disclaimer of Warranty.
    
      THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
    APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
    HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
    OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
    THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
    IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
    ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
    
      16. Limitation of Liability.
    
      IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
    WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
    THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
    GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
    USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
    DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
    PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
    EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGES.
    
      17. Interpretation of Sections 15 and 16.
    
      If the disclaimer of warranty and limitation of liability provided
    above cannot be given local legal effect according to their terms,
    reviewing courts shall apply local law that most closely approximates
    an absolute waiver of all civil liability in connection with the
    Program, unless a warranty or assumption of liability accompanies a
    copy of the Program in return for a fee.
    
                         END OF TERMS AND CONDITIONS
    
                How to Apply These Terms to Your New Programs
    
      If you develop a new program, and you want it to be of the greatest
    possible use to the public, the best way to achieve this is to make it
    free software which everyone can redistribute and change under these terms.
    
      To do so, attach the following notices to the program.  It is safest
    to attach them to the start of each source file to most effectively
    state the exclusion of warranty; and each file should have at least
    the "copyright" line and a pointer to where the full notice is found.
    
        <one line to give the program's name and a brief idea of what it does.>
        Copyright (C) <year>  <name of author>
    
        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.
    
        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.
    
        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
    Also add information on how to contact you by electronic and paper mail.
    
      If the program does terminal interaction, make it output a short
    notice like this when it starts in an interactive mode:
    
        <program>  Copyright (C) <year>  <name of author>
        This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
        This is free software, and you are welcome to redistribute it
        under certain conditions; type `show c' for details.
    
    The hypothetical commands `show w' and `show c' should show the appropriate
    parts of the General Public License.  Of course, your program's commands
    might be different; for a GUI interface, you would use an "about box".
    
      You should also get your employer (if you work as a programmer) or school,
    if any, to sign a "copyright disclaimer" for the program, if necessary.
    For more information on this, and how to apply and follow the GNU GPL, see
    <http://www.gnu.org/licenses/>.

      The GNU General Public License does not permit incorporating your program
    into proprietary programs.  If your program is a subroutine library, you
    may consider it more useful to permit linking proprietary applications with
    the library.  If this is what you want to do, use the GNU Lesser General
    Public License instead of this License.  But first, please read
    <http://www.gnu.org/philosophy/why-not-lgpl.html>.
```
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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="VariabilityExperiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-all-plots (word "/Users/nickroxburgh/Library/CloudStorage/OneDrive-TheJamesHuttonInstitute/Documents/Projects/Protein 2.0/Results/NO_Protein_VariabilityResults/NO_Protein_VariabilityResults_" starting-seed ".csv")</postRun>
    <enumeratedValueSet variable="play-end-sound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="play-step-sound?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="param-scenario">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-yr">
      <value value="2020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-end-yr">
      <value value="2050"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-farms-to-sim">
      <value value="40382"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="animal-yield-trajectory">
      <value value="&quot;Constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dist-to-slaughter">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dist-to-dairy">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slaughter-min-capacity">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dairy-min-capacity">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-scenario">
      <value value="&quot;Determined by markets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-baseline-year">
      <value value="2020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-cm?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-pf?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-init-yr">
      <value value="2024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-init-yr">
      <value value="2024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency-step-int-nonmilk">
      <value value="20000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency-step-int-milk">
      <value value="680000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-tax-start-yr">
      <value value="2025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-tax-coverage">
      <value value="&quot;Agriculture &amp; energy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-beef">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-pork">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-lamb">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-chicken">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-eggs">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-raw-milk">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-wool">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-crops">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-beef">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-pork">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-lamb">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-chicken">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-eggs">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-milk-cream">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-yoghurt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-butter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-cheese">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-farms?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-kommuner?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-fylker?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-slaughterhouses?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-farm-slaught-links?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-dairies?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-farm-dairy-links?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-checkpoints?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="highlight-inactive-farms">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-scenario">
      <value value="&quot;Scenario 7&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-scenario">
      <value value="&quot;Scenario 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-cm-meat">
      <value value="4.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-pf-dairy">
      <value value="0.4044"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-pf-egg">
      <value value="1.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-tax-per-tonne">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency-gain-multiplier">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-growth">
      <value value="&quot;Medium&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-response-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-income-viability">
      <value value="-15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cease-farming-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-max-share">
      <value value="53.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-max-share">
      <value value="53.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cf-required-profit-margin">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-factory-capacity">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-factory-dairy-capacity">
      <value value="170000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-factory-egg-capacity">
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MainExperiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-all-plots (word behaviorspace-experiment-name "-" behaviorspace-run-number "-cmsc-" cm-scenario "-pfsc-" pf-scenario "-emcm-" emissions-cm-meat "-empfd-" emissions-pf-dairy "-empfe-" emissions-pf-egg "-tax-" carbon-tax-per-tonne "-multip-" efficiency-gain-multiplier "-popgrow-" population-growth ".csv")</postRun>
    <exitCondition>year &gt; sim-end-yr</exitCondition>
    <enumeratedValueSet variable="play-end-sound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="play-step-sound?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="param-scenario">
      <value value="&quot;Custom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-yr">
      <value value="2020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-end-yr">
      <value value="2050"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-farms-to-sim">
      <value value="40382"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="animal-yield-trajectory">
      <value value="&quot;Constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dist-to-slaughter">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dist-to-dairy">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slaughter-min-capacity">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dairy-min-capacity">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-scenario">
      <value value="&quot;Determined by markets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-baseline-year">
      <value value="2020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-cm?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-pf?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-init-yr">
      <value value="2024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-init-yr">
      <value value="2024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency-step-int-nonmilk">
      <value value="20000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency-step-int-milk">
      <value value="680000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-tax-start-yr">
      <value value="2025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-tax-coverage">
      <value value="&quot;Agriculture &amp; energy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-beef">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-pork">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-lamb">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-chicken">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-eggs">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-raw-milk">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-wool">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-growth-crops">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-beef">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-pork">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-lamb">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-chicken">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-eggs">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-milk-cream">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-yoghurt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-butter">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consum-growth-cheese">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-farms?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-kommuner?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-fylker?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-slaughterhouses?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-farm-slaught-links?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-dairies?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-farm-dairy-links?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-checkpoints?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="highlight-inactive-farms">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-scenario">
      <value value="&quot;Scenario 5&quot;"/>
      <value value="&quot;Scenario 6&quot;"/>
      <value value="&quot;Scenario 7&quot;"/>
      <value value="&quot;Scenario 8&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-scenario">
      <value value="&quot;Scenario 1&quot;"/>
      <value value="&quot;Scenario 2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-cm-meat">
      <value value="2.9"/>
      <value value="4.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-pf-dairy">
      <value value="0.091327"/>
      <value value="0.4044"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="emissions-pf-egg">
      <value value="0.3523"/>
      <value value="1.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-tax-per-tonne">
      <value value="0"/>
      <value value="1000"/>
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency-gain-multiplier">
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-growth">
      <value value="&quot;Low&quot;"/>
      <value value="&quot;Medium&quot;"/>
      <value value="&quot;High&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-response-ratio">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-income-viability">
      <value value="-15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cease-farming-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-max-share">
      <value value="53.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-max-share">
      <value value="53.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cf-required-profit-margin">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cm-factory-capacity">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-factory-dairy-capacity">
      <value value="170000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pf-factory-egg-capacity">
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
