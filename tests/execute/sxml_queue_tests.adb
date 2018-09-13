--
-- @summary Tests for queue implementation
-- @author  Alexander Senier
-- @date    2018-09-12
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

with AUnit.Assertions; use AUnit.Assertions;
with SXML.Generator; use SXML.Generator;
with SXML.Queue;

package body SXML_Queue_Tests is

   procedure Test_Queue_Basic (T : in out Test_Cases.Test_Case'Class)
   is
      package Q is new SXML.Queue (Integer, 10);
      Tmp : Integer;
   begin
      Assert (Q.Is_Empty, "New queue not empty (0)");
      Assert (not Q.Is_Full, "New queue is full (0)");
      Q.Enqueue (1);
      Assert (not Q.Is_Empty, "Queue is empty (1)");
      Assert (not Q.Is_Full, "Queue is full (1)");
      Q.Enqueue (2);
      Assert (not Q.Is_Empty, "Queue is empty (2)");
      Assert (not Q.Is_Full, "Queue is full (2)");
      Q.Enqueue (3);
      Assert (not Q.Is_Empty, "Queue is empty (3)");
      Assert (not Q.Is_Full, "Queue is full (3)");
      Q.Enqueue (4);
      Assert (not Q.Is_Empty, "Queue is empty (4)");
      Assert (not Q.Is_Full, "Queue is full (4)");
      Q.Dequeue (Tmp);
      Assert (Tmp = 1, "Invalid first element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 2, "Invalid second element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 3, "Invalid third element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 4, "Invalid fourth element:" & Tmp'Img);
      Assert (Q.Is_Empty, "New queue not empty (2)");
      Assert (not Q.Is_Full, "New queue is full (5)");
	end Test_Queue_Basic;

   ---------------------------------------------------------------------------

   procedure Test_Queue_Full (T : in out Test_Cases.Test_Case'Class)
   is
      package Q is new SXML.Queue (Integer, 5);
      Tmp : Integer;
   begin
      Assert (Q.Is_Empty, "New queue not empty (0)");
      Assert (not Q.Is_Full, "New queue is full (0)");
      Q.Enqueue (1);
      Q.Enqueue (2);
      Q.Enqueue (3);
      Q.Enqueue (4);
      Q.Enqueue (5);
      Assert (Q.Is_Full, "Queue not full");
      Q.Dequeue (Tmp);
      Assert (Tmp = 1, "Invalid first element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 2, "Invalid second element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 3, "Invalid third element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 4, "Invalid fourth element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 5, "Invalid fifth element:" & Tmp'Img);
      Assert (Q.Is_Empty, "New queue not empty (0)");
      Assert (not Q.Is_Full, "New queue is full (0)");
	end Test_Queue_Full;

   ---------------------------------------------------------------------------

   procedure Test_Queue_Wrap (T : in out Test_Cases.Test_Case'Class)
   is
      package Q is new SXML.Queue (Integer, 5);
      Tmp : Integer;
   begin
      Assert (Q.Is_Empty, "New queue not empty (0)");
      Assert (not Q.Is_Full, "New queue is full (0)");
      Q.Enqueue (1);
      Q.Enqueue (2);
      Q.Enqueue (3);
      Q.Enqueue (4);
      Q.Enqueue (5);
      Assert (Q.Is_Full, "Queue not full");
      Q.Dequeue (Tmp);
      Assert (Tmp = 1, "Invalid first element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 2, "Invalid second element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 3, "Invalid third element:" & Tmp'Img);
      Q.Enqueue(6);
      Q.Enqueue(7);
      Q.Enqueue(8);
      Assert (not Q.Is_Empty, "Queue not empty (1)");
      Assert (Q.Is_Full, "Queue is full (1)");
      Q.Dequeue (Tmp);
      Assert (Tmp = 4, "Invalid fourth element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 5, "Invalid fifth element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 6, "Invalid sixth element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 7, "Invalid seventh element:" & Tmp'Img);
      Q.Dequeue (Tmp);
      Assert (Tmp = 8, "Invalid eighth element:" & Tmp'Img);
      Assert (Q.Is_Empty, "Queue not empty (2)");
      Assert (not Q.Is_Full, "Queue is full (2)");
	end Test_Queue_Wrap;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Queue_Basic'Access, "Basic queue test");
      Register_Routine (T, Test_Queue_Full'Access, "Full queue");
      Register_Routine (T, Test_Queue_Wrap'Access, "Queue wrap");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Queue Tests");
   end Name;

end SXML_Queue_Tests;
