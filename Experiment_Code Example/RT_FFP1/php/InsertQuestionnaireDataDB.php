<?php

include 'connectDB.php';

$PARTID 		= stripslashes(htmlspecialchars($_POST['partID']));
$EXPID 		= stripslashes(htmlspecialchars($_POST['expID']));
$TNAME 		= stripslashes(htmlspecialchars($_POST['tsName']));
$TRIAL 		= stripslashes(htmlspecialchars($_POST['trial']));
$RTS 		= stripslashes(htmlspecialchars($_POST['reactionTime']));
$RESP 		= stripslashes(htmlspecialchars($_POST['respKey']));
$RESPKEY	= stripslashes(htmlspecialchars($_POST['respKeyID']));
$QUEST_NUM 	= stripslashes(htmlspecialchars($_POST['questionNum']));
$RESP_CH	= stripslashes(htmlspecialchars($_POST['respCh']));

$stmt = $db->prepare("INSERT INTO RT_SMSQ VALUE(?,?,?,?,?, ?,?,?,?, NOW())");
$stmt->bind_param("sssidisii",
$PARTID,$EXPID,$TNAME,$TRIAL,
$RTS,$RESP,$RESPKEY,
$QUEST_NUM, $RESP_CH);
$stmt->execute();
$err = $stmt->errno ;
$data = array(
      'error' => $err,
    );
$stmt->close();
 $db->close();
echo json_encode($data);
 ?>
