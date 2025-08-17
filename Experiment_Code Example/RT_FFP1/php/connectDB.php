<?php



$database="risc_human_rl";
$host="mysql-risc.alwaysdata.net";
$user="risc_human_rl";
$password="PXyMGjq9QE";

$db = new mysqli($host, $user, $password, $database);

if (mysqli_connect_errno()) {
   printf("DB error: %s", mysqli_connect_error());
   exit();
}

?>