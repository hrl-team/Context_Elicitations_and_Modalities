<?php

include 'connectDB.php';

$PARTID 		= stripslashes(htmlspecialchars($_POST['partID']));
$EXPID 		= stripslashes(htmlspecialchars($_POST['expID']));
$TNAME 		= stripslashes(htmlspecialchars($_POST['tsName']));
$TORDER		= stripslashes(htmlspecialchars($_POST['taskOrder']));
$TRIAL 		= stripslashes(htmlspecialchars($_POST['trial']));
$STIM		= stripslashes(htmlspecialchars($_POST['stim']));
$BLOCK 		= stripslashes(htmlspecialchars($_POST['block']));
$TFDB 		= stripslashes(htmlspecialchars($_POST['tsFeedback']));
$TVALS 		= stripslashes(htmlspecialchars($_POST['tsVals']));
$TCHOICE 		= stripslashes(htmlspecialchars($_POST['tsChoice']));
$TBUTTONS		= stripslashes(htmlspecialchars($_POST['tsButtons']));
$TFDBT		= stripslashes(htmlspecialchars($_POST['tsFeedbackTime']));
$TVALST		= stripslashes(htmlspecialchars($_POST['tsValsTime']));
$TBORDERT		= stripslashes(htmlspecialchars($_POST['tsBorderTime']));
$TRANDOM 		= stripslashes(htmlspecialchars($_POST['tsRandom']));
$RTS 		= stripslashes(htmlspecialchars($_POST['reactionTime']));
$RESP 		= stripslashes(htmlspecialchars($_POST['respKey']));
$RESPKEY	= stripslashes(htmlspecialchars($_POST['respKeyID']));
$SYM_CH 	= stripslashes(htmlspecialchars($_POST['symbolChosen']));
$SYMID_CH 	= stripslashes(htmlspecialchars($_POST['symbolChosenID']));
$PROB_CH	= stripslashes(htmlspecialchars($_POST['probChosen']));
$REW_CH 		= stripslashes(htmlspecialchars($_POST['rewardChosen']));
$LOSS_CH	= stripslashes(htmlspecialchars($_POST['lossChosen']));
$RAND_CH 		= stripslashes(htmlspecialchars($_POST['randomChosen']));
$OUT_CH		= stripslashes(htmlspecialchars($_POST['outcomeChosen']));
$SYM_U		= stripslashes(htmlspecialchars($_POST['symbolUnchosen']));
$SYMID_U 	= stripslashes(htmlspecialchars($_POST['symbolUnchosenID']));
$PROB_U	= stripslashes(htmlspecialchars($_POST['probUnchosen']));
$REW_U 		= stripslashes(htmlspecialchars($_POST['rewardUnchosen']));
$LOSS_U 		= stripslashes(htmlspecialchars($_POST['lossUnchosen']));
$RAND_U     = stripslashes(htmlspecialchars($_POST['randomUnchosen']));
$OUT_U 		= stripslashes(htmlspecialchars($_POST['outcomeUnchosen']));
$TOTALREW 		= stripslashes(htmlspecialchars($_POST['totalReward']));
$CHOICE_TYPE		= stripslashes(htmlspecialchars($_POST['choiceType']));


$stmt = $db->prepare("INSERT INTO RT_Preference VALUE(?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,  NOW())");
$stmt->bind_param("ssssiiiiiiiiiiidisiiiiidiiiiiidiii",
$PARTID,$EXPID,$TNAME,$TORDER,
$TRIAL,$STIM,$BLOCK,
$TFDB,$TVALS,$TCHOICE,$TBUTTONS,
$TFDBT,$TVALST,$TBORDERT, $TRANDOM,
$RTS,$RESP,$RESPKEY,
$SYM_CH,$SYMID_CH, $PROB_CH, $REW_CH,$LOSS_CH,$RAND_CH,$OUT_CH,
$SYM_U,$SYMID_U, $PROB_U,$REW_U, $LOSS_U, $RAND_U, $OUT_U,
$TOTALREW,$CHOICE_TYPE
);
$stmt->execute();
$err = $stmt->errno ;
$data = array(
      'error' => $err,
    );
$stmt->close();
 $db->close();
echo json_encode($data);
 ?>
