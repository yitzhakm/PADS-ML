(***********************************************************************
*                                                                      *
*             This software is part of the padsml package              *
*           Copyright (c) 2006-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*            Yitzhak Mandelbaum <yitzhak@research.att.com>             *
*                 David Walker <dpw@cs.princeton.edu>                  *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                  Kenny Zhu <kzhu@cs.princeton.edu>                   *
*                                                                      *
***********************************************************************)
   (* NOTES on new built-in types.
      Pextern (url : string): An external data source found at the specified url.
      Plist_timed: A Plist where the elements are parsed according to the provided schedule. 
                   Assumes no separators and termination is based on the schedule.
      Schedule: an module encapsulating the notion of a schedule.
      Repeat: a sub-module supporting endlessly repeating schedules.
      Plist_dep : A list with no separators or terminators, that "maps" the argument type to the 
                 argument list to produce the new list. List length depends on the argument list. 
                 Also, if the argument list is type 'a list, then the list elements must have have 
                 type 'a -> 'b parser. The parsed list will have type 'b list.
                 Note that, while we can support this in pads/ml, it goes beyond the capabilities of DDC_alpha,
                 as the argument type is higher-order.
    *)

   (* A name of a planet-lab node. *)
   ptype Node_name = Pstring_SE(:Peor:)

   (* A single line/entry in a top file. *)
   ptype Top_record = ...

   (* A single top file. *)
   ptype Top_file = {
     header : Top_header;
     blank_line;
	 column_labels;
	 Top_record Plist_np;
   }

   (******************** Distributed collection *******************)

   (* A function to construct a proper URL for the 
      top file based on the node (host) name. *)
   let make_top_url node_name = ...

   (* A top feed: a remote file, refreshed every 5 minutes. *)
   ptype Top_feed (node_name : string) = 
      Top_file Pextern(make_top_url node_name) Plist_timed(Schedule.Repeat.of_minutes(5))

   (* A configuration file for the collection. *)
   ptype Config_file = {
	nodes: Node_name Precord Plist_np;
   }

   (* A collection of top feeds. The particular members of the collection
      are determined by the contents of the config file. *)
   ptype Top_collection(cfg_url: string) = {
     configuration : Config_file Pextern(cfg_url);
     node_tops     : Top_feed Plist_map (configuration.nodes);
   }

   (********************* Local collection ***********************)

   (* A function to construct a proper file URL for the 
      top file based on the node (host) name. 
    *)
   let make_top_file_url node_name = ...

   (* Snapshot of a top feed: one instance of top data. *)
   ptype Top_snapshot (node_name : string) = 
      Top_file Pextern(make_top_file_url node_name)

   ptype Description_file = {
     num_snapshots : Pint;
     snapshots: Timestamp Precord Plist_sz(num_snapshots);
     nodes: Node_name Precord Plist_np;
   }

   ptype Top_collection_snapshot(descr : Description_file.rep) = {
     node_tops   : Top_snapshot Plist_map (descr.nodes);
   }

   (* A collection of top feeds archived locally. Contains collection of 
      subdirectories named by timestamp. Each subdirectory has all of the 
      top files collected for that time. 
      The particular members of the collection are determined by the contents 
      of the config file. 
    *)
   ptype Top_archive(descr_name: string) = {
     description : Description_file Pfile(descr_name);
     snapshots   : Top_collection_snapshot Plist_map (description.snapshots);
   }
  
