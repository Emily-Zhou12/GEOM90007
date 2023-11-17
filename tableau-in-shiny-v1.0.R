# GEOM90007 Information Visualisation
# tableau-in-shiny R Library
# version 1.0, 2023-09-07
#
# Provides functions to allow Tableau Public workbooks to easily be embedded
# in a Shiny app using the Tableau Embedding API v3.
#
# Written by Alan Thomas
#
# Copyright 2023 The University of Melbourne
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the “Software”), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


# You need to install the shinyjs package before you can use this!
library(shinyjs)

# Call this function from navbarPage(header=...) or anywhere else suitable
# in the Shiny UI
setUpTableauInShiny <- function() {
  registerInputHandler("tinsdf", function(x, session, inputname) {
    jsonlite::fromJSON(x)
  }, force=TRUE)
  
  list(
    useShinyjs(),
    HTML('<script type="module">
      // Import all Tableau objects into the global namespace
      import * as T from "https://public.tableau.com/javascripts/api/tableau.embedding.3.latest.js";
      Object.assign(window, T);
      
      // Map a few common Tableau JS events to Shiny R events
      window.observeTableauEvents = id => {
        const viz = document.getElementById(id);
        viz.addEventListener(TableauEventType.MarkSelectionChanged, async e => {
          const marks = await e.detail.getMarksAsync();
          const columnNames = marks.data[0].columns.map(col => col.fieldName);
          const dataTable = marks.data[0].data.map(row => row.map(val => val.value));
          const dataForShiny = dataTable.map(row => 
            Object.fromEntries(columnNames.map((_, i) => [columnNames[i], row[i]])));
          Shiny.setInputValue(id + "_mark_selection_changed:tinsdf", JSON.stringify(dataForShiny));
        });
        viz.addEventListener(TableauEventType.FilterChanged, async e => {
          const filter = await e.detail.getFilterAsync();
          Shiny.setInputValue(id + "_filter_changed", {
            fieldName: e.detail.fieldName,
            isAllSelected: filter.isAllSelected,
            appliedValues: filter.appliedValues.map(app => app.value)
          });
        });
        viz.addEventListener(TableauEventType.ParameterChanged, async e => {
          const param = await e.detail.getParameterAsync();
          Shiny.setInputValue(id + "_parameter_changed", {
            name: param.name,
            fieldName: param.id,
            value: param.currentValue.value
          });
        });
        console.log("added events for", id)
      };
    </script>')
  )
}

# For inserting a viz into the Shiny UI
tableauPublicViz <- function(id, url, height="500px", style=NA, ...) {
  list(
    tag('tableau-viz', list(id=id,
                            src=url,
                            style=paste0('height: ', height, ';', if (is.na(style)) '' else style),
                            ...)),
    HTML(sprintf('<script>document.addEventListener("DOMContentLoaded", () => { observeTableauEvents("%s"); }, false);</script>', id))
  )
}
