app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$setInputs(sideBar = "taxonomic_tab")
app$setInputs(`taxonomic_ui_1-plotly_bubble_ui_2-field_selection_ui_1-group` = "taxonomic")
app$snapshot()
