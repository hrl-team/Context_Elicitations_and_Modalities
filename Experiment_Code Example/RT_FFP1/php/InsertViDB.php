<?php

include 'connectDB.php';

$PARTID 		= stripslashes(htmlspecialchars($_POST['partID']));
$PROLIFICID 		= stripslashes(htmlspecialchars($_POST['prolificID']));
$PARTVI 		= stripslashes(htmlspecialchars($_POST['partVi']));



$stmt = $db->prepare("INSERT INTO RT_vi VALUE(?,?,?)");
$stmt->bind_param("sss",
    $PARTID,$PROLIFICID,$PARTVI
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
