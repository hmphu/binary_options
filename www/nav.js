$(document).ready(function() {
  
    $( "#BUT_startsim" ).click(function( event ) {
          $("a[data-value=\'Simulation\']").trigger("click");
    });
    
    $( "#BUT_res" ).click(function( event ) {
          $("a[data-value=\'next day prediction\']").trigger("click");
    });
    
    $("#k_multiple").bind("DOMSubtreeModified",function(){
        var stri  = $( "#k_multiple" ).text();
        var perf = stri.replace( /^\\D+/g, "");
        perf = parseInt(perf);
        if (perf <= 11) {
           $("#BIN_MULTIPLE").css("cssText", "background-color: #c9302c !important;");

        } else {
           $("#BIN_MULTIPLE").css("cssText", "background-color: #286090 !important;");
        }
        
    });

});