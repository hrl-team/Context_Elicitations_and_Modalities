<?php

include 'connectDB.php';

$PARTID 		= stripslashes(htmlspecialchars($_POST['partID']));
$EXPID 		= stripslashes(htmlspecialchars($_POST['expID']));
$TNAME 		= stripslashes(htmlspecialchars($_POST['tsName']));
$TRIAL 		= stripslashes(htmlspecialchars($_POST['trial']));

$TBORDERT		= stripslashes(htmlspecialchars($_POST['tsBorderTime']));

$RTS 		= stripslashes(htmlspecialchars($_POST['reactionTime']));
$RESP 		= stripslashes(htmlspecialchars($_POST['respKey']));
$RESPKEY	= stripslashes(htmlspecialchars($_POST['respKeyID']));

$SYMBOL 	= stripslashes(htmlspecialchars($_POST['symbol']));
$SYMBOLID 	= stripslashes(htmlspecialchars($_POST['symbolID']));
$PROB_SYM	= stripslashes(htmlspecialchars($_POST['symbolProb']));
$REW_SYM		= stripslashes(htmlspecialchars($_POST['symbolReward']));
$REW_CH	= stripslashes(htmlspecialchars($_POST['rewardChosen']));
$REW_COR 		= stripslashes(htmlspecialchars($_POST['rewardCorrect']));
$RESP_COR	= stripslashes(htmlspecialchars($_POST['respCorrect']));

$TOTALREW 		= stripslashes(htmlspecialchars($_POST['totalReward']));


$stmt = $db->prepare("INSERT INTO RT_Valuation VALUE(?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?, NOW())");
$stmt->bind_param("sssiidisiiiiiiii",
    $PARTID,$EXPID,$TNAME,$TRIAL,$TBORDERT,
    $RTS ,$RESP,$RESPKEY,
    $SYMBOL,$SYMBOLID,$PROB_SYM,$REW_SYM,$REW_CH,$REW_COR,
    $RESP_COR, $TOTALREW
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
