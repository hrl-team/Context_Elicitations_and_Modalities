import {orderFunc} from "../../general/order.js";

export class Instructions {

    constructor({maxPage, textInstructions,nextText}={})

    {
      this.maxPage = maxPage;
      this.textInstructions = textInstructions ;
      this.nextText = nextText; // what text to display on the final next button
    }

    init(test){
      var buttonNext = '<input align="center" type="button"  class="btn btn-default invisible myBtn" id="bNext" value="Next" >';
      var buttonBack = '<input align="center" type="button"  class="btn btn-default invisible myBtn" id="bBack" value="Back" >';

       $('#Bottom').html(buttonBack+buttonNext);
       let object = this;

      showInstructions(0,object,test)
    }
  }

function showInstructions(page,object,test){
        let titleInstructions = '<H2 align = "center">Instructions (' + page + '/'+ object.maxPage + ')<br></H2>';
        let textInstructions =  '<div class="col"><p align = "center"><br>'+object.textInstructions[page]+'</p></div>'

         $('#Stage').html(textInstructions);
         $('#Top').html(titleInstructions);


      // When to display back button - only if there is a page to go back to
        if (page == 0){
          document.getElementById("bBack").className = "btn btn-default m-2 rounded  invisible myBtn";}

        else if (page > 0){
            document.getElementById("bBack").onclick = function(){showInstructions(page-1,object,test)};
            document.getElementById("bBack").value = "Back";
            document.getElementById("bBack").className = "btn btn-default m-2 rounded  visible myBtn";
          }

      // When to display next button - only if there is a next page to go to
        if (page < object.maxPage-1){
          document.getElementById("bNext").onclick = function(){showInstructions(page+1,object,test)};
          document.getElementById("bNext").value = "Next";
          document.getElementById("bNext").className = "btn btn-default m-2 rounded  visible myBtn";
        }

        else if (page == object.maxPage-1){
          document.getElementById("bNext").className = "btn btn-default m-2 rounded  visible myBtn";
          document.getElementById("bNext").value = object.nextText;
          document.getElementById("bNext").onclick = function(){endInstructions(test)};
        }
}

function endInstructions(test){
  $('#Stage,#Vals,#Bottom,#Top').empty();
  orderFunc(test);
}
