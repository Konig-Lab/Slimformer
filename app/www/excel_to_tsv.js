$(document).on('shiny:connected', function(event) {
  console.log("Check for excel...")
  if (!$('#upload_data')[0]) return;
  
  $('#upload_data').on("change", function(e) {
    console.log("Change detected …");
    const file = e.target.files[0];
    if (!file) return;

    const name = file.name.toLowerCase();
    const isExcel =
      name.endsWith(".xlsx") ||
      name.endsWith(".xls") ||
      file.type === "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" ||
      file.type === "application/vnd.ms-excel";
    if (!isExcel) {
      console.log("Non-Excel file, R reads data direct.");
      // wichtig, damit alter Excel-Text nicht „hängen“ bleibt:
      Shiny.setInputValue("excel_tsv", null, { priority: "event" });
      return;
    }

    console.log("Excel detected, convert with SheetJS …");

    const reader = new FileReader();

    reader.onload = function(evt) {
      const data = new Uint8Array(evt.target.result);
      try {
        const workbook = XLSX.read(data, { type: "array" });
        
        const firstSheetName = workbook.SheetNames[0];
        const worksheet = workbook.Sheets[firstSheetName];

        const tsv = XLSX.utils.sheet_to_csv(worksheet, {
          FS: "\t",
          RS: "\n"
        });
        
        Shiny.setInputValue("excel_tsv", tsv, { priority: "event" });
      } catch (err) {
        console.error("Excel parse error:", err);
        alert("Error while reading Excel-file in browser.");
        Shiny.setInputValue("excel_tsv", null, { priority: "event" });
      }
    };

    reader.onerror = function(err) {
      console.error("FileReader error:", err);
      alert("Error while reading file.");
      Shiny.setInputValue("excel_tsv", null, { priority: "event" });
    };

    reader.readAsArrayBuffer(file);
  });
});
