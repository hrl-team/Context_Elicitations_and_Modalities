import {orderFunc} from "./order.js";
import {sendToDB} from "./dExperimentSendToDB.js";
import {points2pounds} from "./dRandomFunctions.js";

function getID(test) {
       	let Title = "<H3> Login </H3>"
      	let Prompt =  '<form>'+
                 '<div class="form-group">'+
                 	'<label for="formPartID">Please enter your Prolific ID</label>'+
                	 '<input type="text" class="form-control" id="formPartID" placeholder="24-digit prolific ID" maxlength="24" style="width: 27ch;">'+
                 	'<div class="invalid-feedback">You must enter your Prolific ID (24 digits)</div>'+
                 '</div>'+
                 '<form>';

       	let Buttons = '<div align="col m-5"><input align="left" type="button"  class="btn btn-default rounded" id="bStart" value="Next" style="background-color: #3C455C; color:#FFFFFF"></div>';

       $('#Top').html(Title);
       $('#Stage').html(Prompt);
       $('#GameButton').html(Buttons);

	let params = new URLSearchParams(location.search);
	test.PID = params.get('PROLIFIC_PID');

       $('#bStart').click(function() {

         if(document.getElementById('formPartID').value.length===24){
            test.ID = document.getElementById('formPartID').value;

                     $('#Top').html("<p></p>");
                     $('#Stage').empty();
                     $('#Vals').empty();
                     $('#GameButton').empty();
                     $('#Bottom').empty();

                     orderFunc(test);
                   }
          else{formPartID.classList.add('is-invalid');}
       })
     }

function end(test){

let rew = 2000 + test.taskFixedFeedbackInterleaved.results.tRew + test.taskRandomFeedback.results.tRew + test.taskValuations.results.tRew


let time = Date.now()-test.startTime;

      // clear everything
        $('#Stage').empty();
        $('#Vals').empty();
        $('#Bottom').empty();
        $('#Top').empty();

	// send final reward to the database
	sendToDB(0,
        	{ partID: test.ID,
            prolificID: test.PID,
          	expID: test.expID,
          	rewardPoints: rew,
          	rewardPounds: points2pounds(rew,test.rate),
		totalTime: time},
        'php/InsertGeneralDataDB.php'
    );


      // Write on the matrix end of the trial
        var Title = '<h3 align = "center" style="color: #3C455C;"> End </h3>'
        var EndText ='<div class="col"><h3 align = "center"> <br> This is the end of the experiment!</h3>  ' +
        '<p align = "center" ><br> Thank you very much for taking part. <br></p>' +
	'<p align = "center" ><b> You won &pound ' + points2pounds(rew,test.rate)  +' </b></p>' +
  '<p align = "center" ><br> This will paid on top of the fixed compensation. </p><br><br>' +
	'<p align = "center" >You might be invited to another study tomorrow where you will repeat the last two tasks from this study.</p>' +
	'<p align = "center" >The values of symbols will remain the same. <br></p><br><br>'


        $('#Top').html('<div class="row justify-content-center">'+Title+'</div>');
        $('#Stage').html('<div class="row justify-content-center">'+EndText+'</div>');

        let Buttons = '<div align="col m-5"><a href='+ test.link +' class="btn btn-default rounded" style="background-color: #3C455C; color:#FFFFFF">Return to Prolific</a></div>';
        $('#GameButton').html(Buttons);

    }

export {end,getID}
