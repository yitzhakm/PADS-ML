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
open Built_ins
ptype ganglia_value_types =
	Ganglia_value_UNKNOWN of 0
	| Ganglia_value_STRING of 1
	| Ganglia_value_UNSIGNED_SHORT of  2
	| Ganglia_value_SHORT of 3
	| Ganglia_value_UNSIGNED_INT of 4
	| Ganglia_value_INT of 5
	| Ganglia_value_FLOAT of 6
	| Ganglia_value_DOUBLE of 7

let make_mod4 n =
  if n/4*4 != n then (n/4 + 1) * 4
  else n
(* type xdr_string is a binary variable_length string *)
ptype xdr_string = {
  size: pb_uint32;
  content: pstring_FW(make_mod4(Int64.to_int size)) 
(*
  content: pstring_FW((Num.ceiling_num((Int64.to_float size)/.4.0)) * 4)
*)
}

ptype xdr_string_n (n:int) = {
  size: [i:pb_uint32 | (Int64.to_int i)<=n];
  content: pstring_FW(make_mod4(Int64.to_int size)) 
(*
  content: pstring_FW((Num.ceiling_num((Int64.to_float size)/.4.0)) * 4)
*)
} 

let pbint2float32 (num:int32): float = 
	let sign = Int32.to_int (Int32.shift_right num 31) in
	let exponent = Int32.to_int (Int32.shift_right (Int32.shift_left num 1) 23) in
	let f = Int32.to_float (Int32.shift_right (Int32.shift_left num 9) 9) in
	let mattissa = 1.0 +. (f /. (10.0 ** (log10 f +. 1.0))) in
	if sign == 1 then -.(ldexp mattissa exponent)
	else (ldexp mattissa exponent)

let pbint2float64 (num:int64): float = 
	let sign = Int64.to_int (Int64.shift_right num 63) in
	let exponent = Int64.to_int(Int64.shift_right (Int64.shift_left num 1) 52) in
	let f = Int64.to_float (Int64.shift_right (Int64.shift_left num 11) 11) in
	let mattissa = 1.0 +. (f /. (10.0 ** (log10 f +. 1.0))) in
	if sign == 1 then -.(ldexp mattissa exponent)
	else (ldexp mattissa exponent)

let float2int32 n = Int32.of_float n (*TODO: this is just a dummy*)
let float2int64 n = Int64.of_float n (*TODO: this is just a dummy*)
let tx_xdr32 = make_rep_transformers pbint2float32 float2int32
let tx_xdr64 = make_rep_transformers pbint2float64 float2int64
ptype xdr_float32 = (pb_int32, pfloat32) ptransform (tx_xdr32)
ptype xdr_float64 = (pb_int64, pfloat64) ptransform (tx_xdr64)

ptype ganglia_gmetric_message = {
  mytype: xdr_string;
  name: xdr_string;
  value: xdr_string;
  units: xdr_string;
  slope: pb_uint32;
  tmax: pb_uint32;
  dmax: pb_uint32
}

ptype ganglia_spoof_header = {
  spoofName: xdr_string;
  spoofIP: xdr_string
}

ptype ganglia_spoof_message = {
  spheader: ganglia_spoof_header;
  gmetric: ganglia_gmetric_message
}

ptype ganglia_message_formats =
    Metric_user_defined of [b: pb_int32 | b = (Int32.of_int 0)] (* gMetric message *)
  | Metric_cpu_num of [b: pb_int32 | b = (Int32.of_int 1)]
  | Metric_cpu_speed of [b: pb_int32 | b = (Int32.of_int 2 )]
  | Metric_mem_total of [b: pb_int32 | b = (Int32.of_int 3)]
  | Metric_swap_total of [b: pb_int32 | b = (Int32.of_int 4)]
  | Metric_boottime of [b: pb_int32 | b = (Int32.of_int 5)]
  | Metric_sys_clock of [b: pb_int32 | b = (Int32.of_int 6)]
  | Metric_machine_type of [b: pb_int32 | b = (Int32.of_int 7)]
  | Metric_os_name of [b: pb_int32 | b = (Int32.of_int 8)]
  | Metric_os_release of [b: pb_int32 | b = (Int32.of_int 9)]
  | Metric_cpu_user of [b: pb_int32 | b = (Int32.of_int 10)]
  | Metric_cpu_nice of [b: pb_int32 | b = (Int32.of_int 11)]
  | Metric_cpu_system of [b: pb_int32 | b = (Int32.of_int 12)]
  | Metric_cpu_idle of [b: pb_int32 | b = (Int32.of_int 13)]
  | Metric_cpu_aidle of [b: pb_int32 | b = (Int32.of_int 14)]
  | Metric_load_one of [b: pb_int32 | b = (Int32.of_int 15)]
  | Metric_load_five of [b: pb_int32 | b = (Int32.of_int 16)]
  | Metric_load_fifteen of [b: pb_int32 | b = (Int32.of_int 17 )]
  | Metric_proc_run of [b: pb_int32 | b = (Int32.of_int 18)]
  | Metric_proc_total of [b: pb_int32 | b = (Int32.of_int 19)]
  | Metric_mem_free of [b: pb_int32 | b = (Int32.of_int 20)]
  | Metric_mem_shared of [b: pb_int32 | b = (Int32.of_int 21)]
  | Metric_mem_buffers of [b: pb_int32 | b = (Int32.of_int 22)]
  | Metric_mem_cached of [b: pb_int32 | b = (Int32.of_int 23)]
  | Metric_swap_free of [b: pb_int32 | b = (Int32.of_int 24)]
  | Metric_gexec of [b: pb_int32 | b = (Int32.of_int 25)]
  | Metric_heartbeat of [b: pb_int32 | b = (Int32.of_int 26)]
  | Metric_mtu of [b: pb_int32 | b = (Int32.of_int 27)]
  | Metric_location of [b: pb_int32 | b = (Int32.of_int 28)]
  | Metric_bytes_out of [b: pb_int32 | b = (Int32.of_int 29)]
  | Metric_bytes_in of [b: pb_int32 | b = (Int32.of_int 30)]
  | Metric_pkts_in of [b: pb_int32 | b = (Int32.of_int 31)]
  | Metric_pkts_out of [b: pb_int32 | b = (Int32.of_int 32)]
  | Metric_disk_total of [b: pb_int32 | b = (Int32.of_int 33)]
  | Metric_disk_free of [b: pb_int32 | b = (Int32.of_int 34)]
  | Metric_part_max_used of [b: pb_int32 | b = (Int32.of_int 35)]
  | Metric_cpu_wio of [b: pb_int32 | b = (Int32.of_int 36)]
  | Metric_bread_sec of [b: pb_int32 | b = (Int32.of_int 37)]
  | Metric_bwrite_sec of [b: pb_int32 | b = (Int32.of_int 38)]
  | Metric_lread_sec of [b: pb_int32 | b = (Int32.of_int 39)]
  | Metric_lwrite_sec of [b: pb_int32 | b = (Int32.of_int 40)]
  | Metric_rcache of [b: pb_int32 | b = (Int32.of_int 41)]
  | Metric_wcache of [b: pb_int32 | b = (Int32.of_int 42)]
  | Metric_phread_sec of [b: pb_int32 | b = (Int32.of_int 43)]
  | Metric_phwrite_sec of [b: pb_int32 | b = (Int32.of_int 44)]
  | Metric_cpu_intr of [b: pb_int32 | b = (Int32.of_int 45)]
  | Metric_cpu_sintr of [b: pb_int32 | b = (Int32.of_int 46)]
  | Metric_mem_arm of [b: pb_int32 | b = (Int32.of_int 47)]
  | Metric_mem_rm of [b: pb_int32 | b = (Int32.of_int 48)]
  | Metric_mem_avm of [b: pb_int32 | b = (Int32.of_int 49)]
  | Metric_mem_vm of [b: pb_int32 | b = (Int32.of_int 50)]
  | Ganglia_num_25_Metrics of [b: pb_int32 | b = (Int32.of_int  51)] 
	(* this should always directly follow the last 25 Metric_* *)
	(* Yemi *)
  | Spoof_metric of [b: pb_int32 | b = (Int32.of_int 4096)] 
  | Spoof_heartbeat of [b: pb_int32 | b = (Int32.of_int 4097)]

(*
ptype ganglia_message_formats = [i : pb_uint32 | ((Int64.to_int i)>=0 && (Int64.to_int i)<=51) 
				or (Int64.to_int i) = 4096 or (Int64.to_int i) = 4097]
*)

ptype ganglia_message (id:ganglia_message_formats) =
  pmatch id with
    Metric_user_defined _ -> Gmetric of ganglia_gmetric_message
  | Spoof_metric _ -> Spmetric of ganglia_spoof_message
  | Spoof_heartbeat _ -> Spheader of ganglia_spoof_header	
  | Metric_cpu_num _ -> Ushort of pb_uint8
  | Metric_cpu_speed _ -> Uint of pb_uint32  
  | Metric_mem_total _ -> Uint1 of pb_uint32  
  | Metric_swap_total _ -> Uint2 of pb_uint32 
  | Metric_boottime _ -> Uint3 of pb_uint32   
  | Metric_sys_clock _ -> Uint4 of pb_uint32  
  | Metric_proc_run _ -> Uint5 of pb_uint32   
  | Metric_proc_total _ -> Uint6 of pb_uint32 
  | Metric_mem_free _ -> Uint7 of pb_uint32   
  | Metric_mem_shared _ -> Uint8 of pb_uint32 
  | Metric_mem_buffers _ -> Uint9 of pb_uint32
  | Metric_mem_cached _ -> Uint10 of pb_uint32 
  | Metric_swap_free _ -> Uint11 of pb_uint32  
  | Metric_heartbeat _ -> Uint12 of pb_uint32  
  | Metric_mtu _ -> Uint13 of pb_uint32        
  | Metric_mem_arm _ -> Uint14 of pb_uint32    
  | Metric_mem_rm _ -> Uint15 of pb_uint32     
  | Metric_mem_avm _ -> Uint16 of pb_uint32    
  | Metric_mem_vm _ -> Uint17 of pb_uint32     
  | Metric_machine_type _ -> Mystr of xdr_string  
  | Metric_os_name _ -> Mystr1 of xdr_string       
  | Metric_os_release _ -> Mystr2 of xdr_string    
  | Metric_gexec _ -> Mystr3 of xdr_string         
  | Metric_location _ -> Mystr4 of xdr_string      
  | Metric_cpu_user _ -> Float of xdr_float32      
  | Metric_cpu_nice _ -> Float1 of xdr_float32      
  | Metric_cpu_system _ -> Float2 of xdr_float32    
  | Metric_cpu_idle _ -> Float3 of xdr_float32      
  | Metric_cpu_aidle _ -> Float4 of xdr_float32     
  | Metric_load_one _ -> Float5 of xdr_float32      
  | Metric_load_five _ -> Float6 of xdr_float32     
  | Metric_load_fifteen _ -> Float7 of xdr_float32  
  | Metric_bytes_in _ -> Float8 of xdr_float32      
  | Metric_bytes_out _ -> Float9 of xdr_float32     
  | Metric_pkts_in _ -> Float10 of xdr_float32       
  | Metric_pkts_out _ -> Float11 of xdr_float32      
  | Metric_part_max_used _ -> Float12 of xdr_float32 
  | Metric_cpu_wio _ -> Float13 of xdr_float32       
  | Metric_bread_sec _ -> Float14 of xdr_float32     
  | Metric_bwrite_sec _ -> Float15 of xdr_float32    
  | Metric_lread_sec _ -> Float16 of xdr_float32     
  | Metric_lwrite_sec _ -> Float17 of xdr_float32    
  | Metric_rcache _ -> Float18 of xdr_float32        
  | Metric_wcache _ -> Float19 of xdr_float32        
  | Metric_phread_sec _ -> Float20 of xdr_float32    
  | Metric_phwrite_sec _ -> Float21 of xdr_float32   
  | Metric_cpu_intr _ -> Float22 of xdr_float32      
  | Metric_cpu_sintr _ -> Float23 of xdr_float32     
  | Metric_disk_total _ -> Double of xdr_float64   
  | Metric_disk_free _ -> Double1 of xdr_float64
  | _ -> Void of ""

ptype ganglia_25metric = {
  key: pb_int32;
  name: xdr_string_n (16);
  tmax: pb_int32;
  mytype: ganglia_value_types;
  units: xdr_string_n (32);
  slope: xdr_string_n (32);
  fmt: xdr_string_n (32);
  msg_size: pb_int32
}

ptype payload_t = {
  which: ganglia_message_formats;
  payload: ganglia_message(which)
}

ptype libpcapFrame_t = {
  packaetCaptureTime: pb_timestamp;
  microSecsSinceCapture: pb_uint32;
  wireSize: pb_uint32;
  fileSize: pb_uint32
}

let sixEle l _ = (length l == 6)
ptype mac_t = pb_uint8 plist(No_sep, Pred_term(sixEle))

ptype ethernet_t = {
  src: mac_t;
  dst: mac_t;
  protocol: [p:pb_uint16 | p = 0x0800]
}

ptype ip_t = {
  versionAndLength: pb_uint8;
  version: [v: puint8 | v = 4] = versionAndLength/16;
  headerlength: puint8 = versionAndLength mod 16;
  tos: pb_uint8;
  totalLength: pb_uint16;
  id: pb_uint16;
  flagsAndOffset: pb_uint16;
  ttl: pb_uint8;
  protocol: [p:pb_uint8 | p = 0x11];
  headerChecksum: pb_uint16;
  src:psbh_ip;
  dst:psbh_ip;
  options: pstring_FW(headerLength*4 - 20)
}

ptype udp_t = {
  srcPort: pb_uint16;
  dstPort: pb_uint16;
  length: pb_uint16;
  checksum: pb_uint16;
}

(*type pos = int*)

ptype packet_t = {
  libpcap: libpcapFrame_t;
  start: int = Pads.get_current_pos pads;
  ethernet: ethernet_t;
  ip: ip_t;
  udp: udp_t;
  payload: payload_t;
  middle: int = Pads.get_current_pos pads;
  padding: pstring_FW(libpcap.wireSize - (middle-start))
}

ptype libcapHeader_t = {
(*
  magicNumber: [a: pb_uint16 | a = 0xa1b2c3d4];
*)
  magicNumberlow: [a: pb_uint16 | a = 0xc3d4];
  magicNumberhigh: [a: pb_uint16 | a = 0xa1b2];
  majorVersion: pb_uint16;
  minorVersion: pb_uint16;
  timeZoneOffset: pb_uint32;
  timeStampAccuracy: pb_uint32;
  snapShotLength: pb_uint32;
  linkLayerType: pb_uint32
}

ptype lipcap_t = {
  h: libcapHeader_t;
  packet: packet_t plist_np
}

ptype source = lipcap_t
