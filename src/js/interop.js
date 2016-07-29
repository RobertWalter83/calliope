//var fs = require('fs');

function setupPorts(app) {

  app.ports.configureAce.subscribe(
    function(theme) {
      var aceContainer = document.getElementById("editor-container");

      if(aceContainer != null) {
        var editor = aceContainer.editor;
        editor.setTheme(theme);
        editor.$blockScrolling = Infinity;

        editor.on('change', function() {
          app.ports.getEditorContent.send(editor.getValue());
        });
      }  
  });

  app.ports.save.subscribe(
    function(calliopeAppState) {

      localStorage.setItem('calliopeAppState', JSON.stringify(calliopeAppState));

      console.log(localStorage.getItem('calliopeAppState'));
     
      /* for reference if we want to use fs
      var fileName = 'latest.json';              
          
      fs.writeFile(fileName, content, 'utf8', function (err) {
          if(err){
              throw err;
          }
          
          alert("The file '" + fileName + "' has been succesfully saved");
          
      });*/
  });
}

