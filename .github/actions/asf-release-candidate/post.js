/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

const fs = require("fs");
const os = require("os");
const core = require("@actions/core");
const { DefaultArtifactClient } = require('@actions/artifact')
const { exec } = require('@actions/exec');

// Sign and publish all release artifacts. If publishing is disabled, we just
// upload all the release candidate artifacts as GitHub workflow artifacts.
// The post-if condition in action.yml ensures this is only ever run if a job
// succeeds.
async function run() {
	try {
		const project_name = core.getInput("project_name", { required: true });
		const svn_username = core.getInput("svn_username", { required: true });
		const svn_password = core.getInput("svn_password", { required: true });

		const artifact_dir = core.getState("artifact_dir");
		const gpg_signing_key_id = core.getState("gpg_signing_key_id");
		const publish = core.getState("publish") === "true";
		const release_version = core.getState("release_version");

		// sign/checksum all artifacts
		const artifacts = fs.readdirSync(artifact_dir, { recursive: true, withFileTypes: true });
		for(const artifact of artifacts) {
			if (artifact.isFile()) {
				// must sign rpms before sha/gpg since rpmsign modifies the RPM
				if (artifact.name.endsWith(".rpm")) {
					await exec("rpmsign", ["--define", `_gpg_name ${ gpg_signing_key_id }`, "--define", "_binary_filedigest_algorithm 10", "--addsign", `${ artifact.parentPath }/${ artifact.name }`]);
				}
				let checksum = "";
				await exec("sha512sum", ["--binary", artifact.name], {
					cwd: artifact.parentPath,
					listeners: {
						stdout: (data) => { checksum += data.toString(); }
					}
				});
				fs.appendFileSync(`${ artifact.name }.sha512`, checksum);
				await exec("gpg", ["--default-key", gpg_signing_key_id, "--batch", "--yes", "--detach-sign", "--armor", "--output", `${ artifact.name }.asc`, artifact.name], {
					cwd: artifact.parentPath
				});
			}
		}

		if (publish) {
			await exec("svn", ["add", artifact_dir]);
			await exec("svn", ["commit", "--username", svn_username, "--password", svn_password, "--message", `Stage ${ project_name } ${ release_version }`, artifact_dir]);
		} else {
			// if publishing was disabled then this action was likely just triggered
			// just for testing, so upload the maven-local and artifact directories so
			// they can be verified
			const release_dir = `${ os.tmpdir() }/release`;
			const upload_artifacts = fs.readdirSync(release_dir, { recursive: true, withFileTypes: true })
				.filter((dirent) => dirent.isFile())
				.filter((dirent) => !dirent.parentPath.split("/").includes(".svn"))
				.map((dirent) => `${ dirent.parentPath }/${ dirent.name }`);
			const artifact_client = new DefaultArtifactClient();
			artifact_client.uploadArtifact(`release`, upload_artifacts, os.tmpdir(), {
				compressionLevel: 0,
				retentionDays: 1
			});
		}

	} catch (error) {
		core.setFailed(error.message);
	}
}

run();
