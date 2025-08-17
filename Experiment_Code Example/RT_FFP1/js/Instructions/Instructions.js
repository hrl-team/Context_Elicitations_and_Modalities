import {Instructions} from "./dict/dInstructions.js";
import {points2pounds}  from "../general/dRandomFunctions.js";

var InstructionsA = new Instructions(
    {maxPage:7,
    nextText: "Practice!",
    textInstructions:[//page 1
                      'This study consists of 3 memory tasks and a short questionnaire. '+
                      'Your bonus payment will depend on how many points you collect in these tasks.'+
                      '<br><br>You will start the study with 2000 points. '+
                      'Depending on your choices, you can double the starting bonus or lose it entirely.' +
                      '<br><br>You can maximize your payoff by choosing options that win more points and avoiding options that lose them. ',

                      //page 2
                      'At the end of the study, we will convert the accumulated points into pounds, '+
                      'and add them to the fixed compensation provided by Prolific.' +
                      '<br><br> The conversion rate is: '+
                          '<br><b> 90 points = winning 6.3 pence. </b>' +
                          '<br><b> 10 points = winning 0.7 pence. </b>'+
                          '<br><b> -10 points =  losing 0.7 pence. </b>'+
                          '<br><b> -90 points =  losing 6.3 pence. </b>'+
                      '<br><br>If you double your starting bonus, you can win up to £5.5 '+
                      'including the fixed compensation by Prolific.' ,

                      //page 3
                      'In the first task, you will have to decide between two symbols displayed on either side of the screen. '+
                      'Each symbol is associated with a different probability of either winning or losing points. '+
 		      '<br><br>The side they are presented on (left/right) does not matter. '+
 		      '<br><br><div style="text-align: center"><img src="images/instructions/fdb_screen1.png" style="padding: 1rem; text-align: center" class = "img-fluid">',


		      //page 4
			'After each choice, you will see the outcome, i.e. how many points '+
                  	  'you have won/lost and how much you could have won/lost had you chosen the other option. '+
                      '<br><br>The possible outcomes are -90, -10 , 0, 10 or 90 points.'+
			'<br><br><div style="text-align: center"><img src="images/instructions/fdb_screen2.png" style="padding: 1rem; text-align: center" class = "img-fluid">',

                      //page 5
                      '<br>The second and third tasks will involve the same symbols as the first task '+
                      'and the associated points and probabilities will be the same in all tasks. '+
                      '<br><br>However, in those tasks, the choice outcomes won&apos;t be shown and '+
                      'you will have to base your choices on what you learnt in the first task.',

                      //page 6
                      '<br>If you complete all tasks, you might be invited to a follow-up study, ' +
                      'where you will repeat the second and the third tasks. ' +
                      'It will open in roughly 24 hours. ',


                      //page 7
                      '<b>You will now start with a few practice trials! </b>'+
                      '<br>Points collected in the those trials, do not count towards the final payoff. ' +
                      'If you want, you can repeat the practice twice. ']
})

var InstructionsB = new Instructions(
    {maxPage:2,
    nextText: "Start the Task!",
    textInstructions:[//page 1
                      '<br>You will now start the second task, where '+
                      'you will have to choose between the symbols you saw in the first task. '+
                      '<br><br>The symbols will have the same values as in the first task, i.e. they will give (on average) the same amount of points, '+
                      'but they will be presented in new combinations.',
			//page 2
                      '<br> This time, we won’t show you the outcomes of your choices but ' +
                      'you will see your final score at the end of the task.' +
                      '<br><br>The final score will depend on the choices you make during the task.'+
                      'The higher the outcomes associated with the symbol, the higher the score.']
})

var InstructionsC = new Instructions(
    {maxPage:1,
    nextText: "Start the Task!",
    textInstructions:[//page 1
                      'You will now start the third task.'+
                      '<br><br>In this task, we will show you symbols you saw in the previous tasks '+
                      'together with 5 possible outcomes (-90, -10, 0, 10, 90 points).'+
                      '<br><br>Please indicate which was the most frequent outcome for each symbol.'+
                      '<br><br>Note that for some symbols, the most common outcome can be 0.'+
                      '<br><br> You will be rewarded 10 points for each correct response, '+
                      'and the total will be shown at the end of the task.' ]
})


var InstructionsSMSQ = new Instructions(
    {maxPage:1,
    nextText: "Start the Questionnaire!",
    textInstructions:[//page 1
                      'The following questionnaire will help us better understand your memory.' +
                      '<br><br> It containts 57 statements. Please read them carefully and '+
                      'indicate the degree to which each statement applies to you. ']
})





export {InstructionsA, InstructionsB,InstructionsC,InstructionsSMSQ}
