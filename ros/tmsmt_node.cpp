/*********************************************************************
 * Software License Agreement (BSD License)
 *
 *  Copyright (c) 2012, Willow Garage, Inc.
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *   * Neither the name of Willow Garage nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 *  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *********************************************************************/
#include <ros/ros.h>

#include "moveit_container.hpp"

int main(int argc, char **argv)
{
    ros::init (argc, argv, "move_group_tutorial");
    ros::NodeHandle node_handle("~");
    container cont(node_handle.getNamespace(), "robot_description");


    double q0[7] = {0};
    cont.set_start( "right_arm", 7, q0 );

    /* Joint Goal */

    // robot_state::RobotState goal_state(robot_model);
    // const robot_state::JointModelGroup* joint_model_group = goal_state.getJointModelGroup(req.group_name);
    // {
    //     std::vector<double> joint_values(7, 0.0);
    //     joint_values[0] = 0.5;
    //     joint_values[3] = 0.5;
    //     joint_values[5] = 0.5;
    //     goal_state.setJointGroupPositions(joint_model_group, joint_values);
    // }
    // moveit_msgs::Constraints joint_goal = kinematic_constraints::constructGoalConstraints(goal_state, joint_model_group);
    // req.goal_constraints.clear();
    // req.goal_constraints.push_back(joint_goal);


    /* Workspace Goal */
    double q[4] = {0.423811, 0.566025, -0.423811, 0.566025};
    double v[3] = {0.363087, -1.278295, 0.320976 + .02};
    cont.set_ws_goal("right_arm", q, v );

    cont.plan();

    return 0;
}