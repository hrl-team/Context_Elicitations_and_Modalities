import {getID,end} from "./dExperimentFunctions.js";
import {disableF5} from "./dRandomFunctions.js";
import {consentForm} from "./dExperimentConsent.js";
import {InstructionsA, InstructionsB,InstructionsC,InstructionsSMSQ} from "../Instructions/Instructions.js"
import {SMSQ} from "../Questionnaires/Questionnaire.js"

function orderFunc (test){
  let check = test.orderCheck;
  switch (check) {

    case 0:
     getID(test)
     test.orderCheck++
     break;
    case 1:
      consentForm.showConsent(consentForm, test);
      test.orderCheck++
      break;
    case 2:
	$(document).on("keydown", disableF5);
      InstructionsA.init(test);
      test.orderCheck++
      break;
    case 3:
      test.trainingFixedFeedback.init(test) ;
      test.orderCheck++
      break;
    case 4:
      test.taskFixedFeedbackInterleaved.init(test) ;
      test.orderCheck++
      break;
    case 5:
      InstructionsB.init(test);
      test.orderCheck++
      break;
    case 6:
      test.taskRandomFeedback.init(test) ;
      test.orderCheck++
      break;
    case 7:
      InstructionsC.init(test);
      test.orderCheck++
      break;
    case 8:
      test.taskValuations.init(test) ;
      test.orderCheck++
      break;
    case 9:
      InstructionsSMSQ.init(test)
      test.orderCheck++
      break;    
   case 10:
      SMSQ.init(test)
      test.orderCheck++
      break;
   case 11:
      end(test);
      break;

    default:
      $('#Stage').html('<p>Error!</p>');

  }
}

export{orderFunc}
