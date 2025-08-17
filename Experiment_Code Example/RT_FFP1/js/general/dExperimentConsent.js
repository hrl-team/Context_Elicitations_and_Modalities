import {orderFunc} from "./order.js";

class Consent {

    constructor({
      study,
      researcher,
      intro,
      aim,
      procedure,
      participation,
      confidentiality,
      publication,
      contact,
      ethics,
      consentForm,
      consentCheck}={})

    {
      this.study = study;
      this.researcher = researcher ;
      this.intro = intro;
      this.aim =aim;
      this.procedure = procedure;
      this.participation = participation;
      this.confidentiality = confidentiality;
      this.publication = publication;
      this.contact= contact ;
      this.ethics = ethics;
      this.consentForm = consentForm;
      this.consentCheck = consentCheck;
    }

    showConsent(object,test){
      document.getElementById("Stage").className = "row justify-content-left";

      let study = "<h5>Title of the Study: </h5>"+"<p><b>"+object.study+"</b></p>";
      let researcher = "<h5>Researcher in Charge: </h5>"+"<p><b>"+object.researcher+"</b></p>";
      let intro = "<p>"+object.intro+"</p>";
      let aim = "<h5>What is the aim of this study?</h5>"+ "<p>"+object.aim+"</p>";
      let procedure = "<h5>What will happen if I take part?</h5>"+ "<p>"+object.procedure+"</p>";
      let participation = "<h5>Can I withdraw from the study?</h5>"+ "<p>"+object.participation+"</p>";
      let confidentiality = "<h5>Confidentiality - Who will access the data?</h5>"+ "<p>"+object.confidentiality+"</p>";
      let publication = "<h5>Research Results And Publication</h5>"+ "<p>"+object.publication+"</p>";
      let contact = "<h5>Contact And Additional Information</h5>"+"<p class='ow'><b>Email: </b>"+object.contact+"</p>";
      let ethics= "<p>"+object.ethics+"</p>";
      let consent = "<h5>Consent</h5>"+ "<p>"+object.consentForm+"</p>";

      var consentCheck = [];
      for (let i = 0; i <= object.consentCheck.length-1; i++){
          consentCheck[i] =
                '<div class="form-check">'+
                '<input align="left" class="form-check-input" type="checkbox" value="" id='+"check"+i+' required>'+
                '<label class="form-check-label" for='+"check"+i+'>' +object.consentCheck[i]+'</label>'+
                '</div>'}

      $('#Top').html("<h4  style='color:#FFFFFF'>Consent</h4>")
      $('#Stage').html('<div class="col" >'+
        study+
        researcher+
        intro+
        aim+
        procedure+
        participation+
        confidentiality+
        publication+
        contact+
        ethics+
        consent+
        consentCheck[0]+
        consentCheck[1]+
        consentCheck[2]+
        '</div>');

  //  $('#Stage').append(consentCheck);


        // next button
        var buttonBack = '<input align="center" type="button"  class="btn btn-default invisible rounded m-2" id="bBack" value="Back"  style="background-color: #3C455C; color:#FFFFFF">';
        var buttonNext = '<input align="center" type="button"  class="btn btn-default rounded visible m-2" id="bNext" value="Next"  style="background-color: #3C455C; color:#FFFFFF">';
        $('#Bottom').html(buttonBack + buttonNext);

        $('#bNext').click(function() {
        if ($("input:checkbox:not(:checked)").length == 0) {
                    let f = "test."+object.followUp
                    $('#Top').html("<p></p>");
                    $('#Stage').empty();
                    $('#Vals').empty();
                    $('#GameButton').empty();
                    $('#Bottom').empty();

                    document.getElementById("Stage").className = "row justify-content-center";

                    orderFunc(test);
                  }
         else{$('#GameButton').html('<div class="alert alert-warning" role="alert">You must check all boxes to continue!</div>');}
      })
    }
  }

var consentForm = new Consent (
    { study: "The domain-general role of reinforcement learning-based training in cognition across short and long time-spans",
      researcher: "Pr. Stefano PALMINTERI",
      intro: "You are being invited to take part in a research study. Before you decide to participate, it is "+
             "important that you understand why the research is being conducted and what it will involve. "+
             "Please take time to read the following information carefully.",

      aim:"This study aims to understand the learning processes in decision-making, in particular the "+
              "cognitive mechanisms of these learning and decision-making processes. "+
              "<br><br>The following experiment has no immediate application or clinical value. "+
              "However, it will allow us to improve our understanding of the functioning brain.",

      procedure:"You will be asked to complete 3 cognitive tasks and a 57-item questionnaire about memory, "+
                "neither or which requires any particular skill or knowledge. " +
                "The study will likely take you 20-25 min. " +
                "If you complete it, you will receive between 2€ - 5€ depending on your choices during the tasks. ",

      participation: "Your participation in this research study is voluntary " +
                    "and you may stop and withdraw at any time without prejudice or justification. ",

    confidentiality:"In addition to your responses, we will also collect " +
                    "<a href='https://researcher-help.prolific.co/hc/en-gb/articles/360009391633-Exporting-Prolific-Demographic-Data' target='_blank'>"+
                    "these demographic data</a> if you provided them to Prolific. "+
                    "In order to preserve your identity and the confidentiality, the identification of "+
                    "each file will be coded, thus preserving the anonymity of your answers."+
                    "<br><br> The collected data will be accessible to the researcher in charge and his staff "+
                    "and will be only used for research purposes in order to answer the scientific objectives of the project. "+
                    "The data may be published in scientific journals and shared within the scientific community, "+
                    "in which case no publication or scientific communication will contain any identifying information. ",

      publication: "You will be able to check the publications resulting from this study on the following "+
                    "<a href='https://sites.google.com/site/stefanopalminteri/publications' target='_blank'>"+
                    "website.</a> ",
      contact: "humanreinforcementlearning@gmail.com",
      ethics: "This research has received a favorable opinion from the Inserm Ethical Review Committee / IRB00003888 on November 13th, 2018",
      consentForm: "Your participation in this research confirms that you have read this information and wish to participate in the research study."+
                    "Please check all boxes to continue:",
      consentCheck: ["I am 18 years old or more",
                    "My participation in this experiment is voluntary",
                    "I understand that my data will be kept confidential and I can stop at any time without justification"]

    })

  export {consentForm}
