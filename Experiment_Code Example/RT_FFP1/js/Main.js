import {consentForm} from "./general/dExperimentConsent.js";
import {orderFunc} from "./general/order.js";

// tasks
import {taskFixedFeedbackInterleaved} from "./tasks/FixedFeedbackInterleaved/TaskFixedFeedbackInterleaved.js";
import {trainingFixedFeedback} from "./tasks/FixedFeedbackTraining/TrainingFixedFeedback.js";
import {taskRandomFeedback} from "./tasks/RandomFeedbackInterleaved/TaskRandomFeedback.js";
import {taskValuations} from "./tasks/Valuations/TaskValuations.js";


window.onload=start();

function start(){
  let order = Math.round(Math.random());

  var experimentFile = {
		expID: 'RT_FFP1',
    link: "https://app.prolific.co/submissions/complete?cc=4B496F95",
    rate: 0.0007,
    order:0,
		orderCheck: 0,
		startTime: Date.now(),
    consentForm,
    taskFixedFeedbackInterleaved,
    trainingFixedFeedback,
    taskRandomFeedback,
	  taskValuations}

setTimeout(function(){orderFunc(experimentFile)},100);
}
