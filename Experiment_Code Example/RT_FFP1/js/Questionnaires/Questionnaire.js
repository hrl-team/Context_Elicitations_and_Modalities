import {sendToDB} from "../general/dExperimentSendToDB.js";
import {orderFunc} from "../general/order.js";

var SMSQ =
  { taskName: "SMSQ",
    maxTrials:57,
    questionItems:["I believe I recall the details of certain events better than others do. ",
                  "I always remember to bring things I have promised. ",
                  "Generally speaking, I am forgetful. ",
                  "It often happens that I come to the store and can’t remember all the things I wanted to buy. ",
                  "I have noticed that I have better memory than others ",
                  "I wish I had a better memory. ",
                  "I have better memory than most of my peers. ",
                  "I am always able to memorize things I really want to remember",
                  "I can memorize large amount of information in a short time period.",
                  "My memory is as good as it was 5 years ago.",
                  "People who know me believe I have a good memory.",
                  "When disputable, I rely on my own more often than on somebody else’s memory.",
                  "It rarely happens to me that a name or a word is ‘on the tip of my tongue’ and that I can’t remember it.",
                  "I believe memory is important. ",
                  "I immediately memorize names of the people I get acquainted with.",
                  "I remember most of the names and surnames of my primary school classmates.",
                  "I remember the dates of important events in my life.",
                  "I can always easily remember what I wore the day before.",
                  "I can easily remember where I have spent the New Year’s Eve in last 5 years.",
                  "I remember shop-window in town where I have seen something.",
                  "I know the birthday dates of most of my friends.",
                  "I can easily associate a face to a name",
                  "I remember the name and the face of my first primary school teacher.",
                  "I rarely forget the time and the place where I am to meet someone.",
                  "I do not forget to congratulate birthdays to people who are close to me.",
                  "I can easily remember what I was doing on a certain day during the last week.",
                  "I correctly remember the important addresses (friends, work,university, parents etc.).",
                  "It is enough for me to hear a new word once and remember it.",
                  "In school, it was never a problem for me to learn a poem by heart.",
                  "I easily memorize lyrics.",
                  "I can easily remember the chorus of the song I just heard.",
                  "If I read the morning newspapers, I can tell the headlines easily in my own words that afternoon.",
                  "I have difficulties in remembering the names of the characters in the book I recently read.",
                  "I remember the names of the authors of the books I’ve read.",
                  "I remember the stories I have heard in my childhood.",
                  "I have a good recollection of the plots in the movies I have seen.",
                  "When I watch a TV-show I can always remember what happened in previous episode.",
                  "Even if I forget to bookmark the page of the book I am reading, I can easily find the page I was on.",
                  "When I watch a movie, I easily remember even the less important characters.",
                  "I have good memory for jokes.",
                  "I know lots of quotations and sayings.",
                  "When I watch a movie, I can easily recall other movies in which I have seen the same actors.",
                  "I can say a seven-digit number backwards when I hear it.",
                  "I can easily memorize the time schedule of a bus or a tram.",
                  "I can do calculations ‘in my head’ relatively easy.",
                  "I easily memorize a new telephone number without writing it.",
                  "I know almost all the important telephone numbers by heart.",
                  "I have good memory for numbers.",
                  "When I am in new city, I easily recall the places I have passed by.",
                  "I remember the faces of the people when I first meet them.",
                  "I easily recognize pictures I have seen before.",
                  "I have a good recollection for people’s faces.",
                  "I can return to the route that I have passed through only once, even after a longer period of time.",
                  "I often make ‘to do’ lists.",
                  "I use reminders for everyday activities on regular basis.",
                  "If I have to buy more than four or five things, I make a list.",
                  "I often use some form of alarm as a reminder of an activity in the near future (alarm clock, timer, mobile phone etc.)."],
    questionResponses:["Strongly Disagree","Disagree","Neutral","Agree", "Strongly Agree"],
    ri: [...Array(5).keys()],
    qi: [...Array(57).keys()],

    rs:{
      rt: [],
      rt_point: [],

      questionNum: [],
      questionVerbal: [],

      respKey: [],
      respKeyID: [],

      respCh: [],
      respChVerbal: [],

      respCor: [],

      trial: 0
    },

     init: function(test) {
	let object = this;
       var buttonNext = '<input align="center" type="button"  class="btn btn-default invisible" id="bNext" value="Next"  style="background-color: #3C455C; color:#FFFFFF">';
       var buttonBack = '<input align="center" type="button"  class="btn btn-default invisible" id="bBack" value="Back"  style="background-color: #3C455C; color:#FFFFFF">';

       $('#Bottom').html(buttonBack+buttonNext);

       showQuestion(this,test);
	  }
}


function showQuestion(object,test){
if(object.rs.trial==(object.maxTrials)){endTask(object,test);return}
//if(object.rs.trial==0){shuffle(object.qi)}

// record time
object.rs.rt_point.push(Date.now())


//  shuffle(object.ri)

  let title= '<H2 align = "center">Question (' + object.rs.trial + '/'+ object.maxPage + ')<br></H2>';

  let question=  '<div class="col"><p align = "justify"><br>'+object.questionItems[object.qi[object.rs.trial]]+'</p></div>'

  let resp0 = makeRadioPoint("SMSQ","myResp0","0",object.questionResponses[object.ri[0]])
  let resp1 = makeRadioPoint("SMSQ","myResp1","1",object.questionResponses[object.ri[1]])
  let resp2 = makeRadioPoint("SMSQ","myResp2","2",object.questionResponses[object.ri[2]])
  let resp3 = makeRadioPoint("SMSQ","myResp3","3",object.questionResponses[object.ri[3]])
  let resp4 = makeRadioPoint("SMSQ","myResp4","4",object.questionResponses[object.ri[4]])

  $('#Top').html(title);
  $('#Stage').html(question);
  $('#Vals').html('<form>'+resp0+resp1+resp2+resp3+resp4+'<form>');

  if (object.rs.trial  < object.maxTrials){
    document.getElementById("bNext").onclick = function(){recordResponses(object,test)};
    document.getElementById("bNext").value = "Next";
    document.getElementById("bNext").className = "btn btn-default m-2 rounded  visible";
  }

}

function makeRadioPoint (rName,rId,rValue,rLabel){
  let radio = '<div class="form-check">'+
  '<input class="form-check-input" type="radio" name='+rName+ ' id=' +rId+ ' value=' +rValue+'>'+
  '<label class="form-check-label" for='+rId+'>'+  rLabel + '</label></div>'
  return radio
}

function recordResponses (object,test){
if($("input:radio:checked").length == 1) {
    object.rs.rt.push(Date.now()-object.rs.rt_point[object.rs.rt_point.length-1]);

    let respKey = document.querySelector('input[name="SMSQ"]:checked').value;
    let respKeyID = document.querySelector('input[name="SMSQ"]:checked').id;

    let questionVerbal = object.questionItems[object.qi[object.rs.trial]]
    let questionNum = object.qi[object.rs.trial]

    let respCh = object.ri[respKey];
    let respChVerbal = document.querySelector('label[for='+respKeyID+']').innerHTML

  //  console.log([object.rs.trial,questionNum,questionVerbal,respKey,respKeyID,respChVerbal,respCh, respCor])

    sendToDB(0,
        { partID: test.ID,
          expID: test.expID,
          tsName: object.taskName,
          trial: object.rs.trial,
          questionNum: questionNum,
          reactionTime: object.rs.rt[object.rs.rt.length-1],
          respKey: respKey,
          respKeyID: respKeyID,
          respCh: respCh
        },
        'php/InsertQuestionnaireDataDB.php'
    );

    object.rs.trial++

    $('#GameButton').empty();
    showQuestion(object,test)
  }
    else{$('#GameButton').html('<div class="alert alert-warning" role="alert">You must select a response to continue</div>');}
}


function endTask(object,test){
  // clear everything
    $('#Stage').empty();
    $('#Vals').empty();
    $('#Bottom').empty();


  // Write on the matrix end of the trial
    var EndText ='<div class="col"><h5 align = "center"> <br> This is the end of the task!<br> <br> </h5><br>   '
    var EndBut = '<div align="center"><input type="button"  class="btn btn-default" id="bEnd" value="Please click here to continue" style="background-color: #3C455C; color:#FFFFFF"></div>';

    $('#Stage').html('<div class="row justify-content-center">'+EndText+'</div>');
    $('#GameButton').html('<div class="row justify-content-center">'+EndBut+'</div>');

    document.getElementById("bEnd").onclick = function(){

            $('#Stage').empty();
            $('#Vals').empty();
            $('#GameButton').empty();
            $('#Bottom').empty();
            orderFunc(test);
          };
}

function shuffle(array) {
  let counter = array.length;

  /* While there are elements in the array */
  while (counter > 0) {
    /* Pick a random index */
    let index = Math.floor(Math.random() * counter);

    /* Decrease counter by 1 */
    counter--;

    /* And swap the last element with it */
    let temp = array[counter];
    array[counter] = array[index];
    array[index] = temp; }}

export{SMSQ}
