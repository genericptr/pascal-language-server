'use strict';

// import * as path from 'path';
// import * as fs from 'fs';

import {
	TextDocument,
	TextEdit,
	TextEditor,
	Position,
	Range,
	CodeAction,
	CodeActionProvider,
	CodeActionKind,
	window,
	commands,
	workspace,
	ExtensionContext,
	WorkspaceFolder,
	languages,
	Uri,
	Command,
	TextEditorDecorationType,
	DecorationRangeBehavior,
	
} from 'vscode';
import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	NotificationType
} from 'vscode-languageclient';
import { StreamMessageReader, StreamMessageWriter } from 'vscode-languageserver-protocol';
import * as fs from 'fs';
import * as net from 'net';
import { 
	InputRegion ,
	DecorationRangesPair,
   InactiveRegionParams
} from './servermessages';

const CompleteCommand = 'pasls.completeCode';
const InvokeCompleteCommand = 'invoke.codeCompletion';

const FormatCommand = 'pasls.formatCode';
const InvokeFormatCommand = 'invoke.formatCode';

const InvertAssignmentCommand = 'pasls.invertAssignment';
const InvokeInvertAssignmentCommand = 'invoke.invertAssignment';

const RemoveEmptyMethodsCommand = 'pasls.removeEmptyMethods'; 
const InvokeRemoveEmptyMethodsCommand = 'invoke.removeEmptyMethods'; 

const RemoveUnusedUnitsCommand = 'pasls.removeUnusedUnits';
const InvokeRemoveUnusedUnitsCommand = 'invoke.removeUnusedUnits';

// const InactiveRegionNotification = 'pasls.inactiveRegions';
const InactiveRegionNotification: NotificationType<InactiveRegionParams> = new NotificationType<InactiveRegionParams>('pasls.inactiveRegions');

let client: LanguageClient;
let completecmd: Command;
let inactiveRegionsDecorations = new Map<string, DecorationRangesPair>();
let tcpSocket: net.Socket | undefined;
    
function invokeFormat(document: TextDocument, range: Range) {
	let activeEditor = window.activeTextEditor;
	if (!activeEditor) {
		window.showErrorMessage('No active editor.')
		return;
	}

	// Do we have a document ?
	let doc = document ? document : (activeEditor ? activeEditor.document : undefined);
	if (!doc) {
		window.showErrorMessage('No document available.')
		return;
	}
	let fn: string = doc ? doc.uri.fsPath : '';
	if (!fn) {
		window.showErrorMessage('Documents needs to be saved first.')
		return;
	}
	// Maybe check for extensions ? 
	if (doc.isDirty) doc.save();

	let formatConfig: string = workspace.getConfiguration('pascalLanguageServer').get('formatConfig') || '';
	if (formatConfig) {
		formatConfig = 'file://' + formatConfig;
		if (!fs.existsSync(formatConfig)) {
			window.showErrorMessage('Formatter config file does not exist: ' + formatConfig);
			return;
		}
	}

	if (doc.uri) {
		commands.executeCommand(FormatCommand, doc.uri.with({ "scheme": "file" }).toString(), formatConfig);
	}
}

function invokeRemoveEmptyMethods() {
	// Do we have a document ?
	let activeEditor = window.activeTextEditor;
	if (!activeEditor) {
		return;
	}
	
	let doc : TextDocument = activeEditor.document;
	if (!doc) {
		window.showErrorMessage('No document available.')
		return;
	}

	let pos : Position = activeEditor.selection.start;
	
	if (doc.uri) {
		commands.executeCommand(RemoveEmptyMethodsCommand, doc.uri.with({ "scheme": "file" }).toString(), pos);
	}
}



function invokeRemoveUnusedUnits() {
    // Do we have a document ?
    let activeEditor = window.activeTextEditor;
    if (!activeEditor) {
        return;
    }
    let doc : TextDocument = activeEditor.document;
        
    if (!doc) {
        window.showErrorMessage('No document available.');
        return;
    }
    let pos : Position = activeEditor.selection.start;
    if (doc.uri) {
        commands.executeCommand(RemoveUnusedUnitsCommand, doc.uri.with({ "scheme": "file" }).toString(), pos);
    }
}

function invokeInvertAssignment(document: TextDocument, range: Range) {
	// Do we have a document ?
	let activeEditor = window.activeTextEditor;
	if (!activeEditor) {
		return;
	}
	let sPos = activeEditor.selection.start;
	let ePos = activeEditor.selection.end;
	let doc : TextDocument = document ? document : activeEditor.document;

	if (!doc) {
		window.showErrorMessage('No document available.')
		return;
	}
	
	if (doc.uri) {
		commands.executeCommand(InvertAssignmentCommand, doc.uri.with({ "scheme": "file" }).toString(), sPos, ePos);
	}	
}

function setInactiveRegion(params: InactiveRegionParams) {

	//const settings: CppSettings = new CppSettings(this.RootUri);
	const opacity: number | undefined = 0.3;//settings.inactiveRegionOpacity;
	if (opacity !== null && opacity !== undefined) {
			let backgroundColor: string | undefined = "";//settings.inactiveRegionBackgroundColor;
			if (backgroundColor === "") {
					backgroundColor = undefined;
			}
			let color: string | undefined = "";//settings.inactiveRegionForegroundColor;
			if (color === "") {
					color = undefined;
			}
			const decoration: TextEditorDecorationType = window.createTextEditorDecorationType({
					opacity: opacity.toString(),
					backgroundColor: backgroundColor,
					color: color,
					rangeBehavior: DecorationRangeBehavior.OpenOpen
			});
			// We must convert to vscode.Ranges in order to make use of the API's
			const ranges: Range[] = [];
			params.regions.forEach(element => {
					const newRange: Range = new Range(element.startLine-1, element.startCol-1, element.endLine-1, element.endCol-1);
					ranges.push(newRange);
			});
			// Find entry for cached file and act accordingly
			const valuePair: DecorationRangesPair | undefined = inactiveRegionsDecorations.get(params.uri);
			if (valuePair) {
					// Disposing of and resetting the decoration will undo previously applied text decorations
					valuePair.decoration.dispose();
					valuePair.decoration = decoration;
					// As vscode.TextEditor.setDecorations only applies to visible editors, we must cache the range for when another editor becomes visible
					valuePair.ranges = ranges;
			} else { // The entry does not exist. Make a new one
					const toInsert: DecorationRangesPair = {
							decoration: decoration,
							ranges: ranges
					};
					inactiveRegionsDecorations.set(params.uri, toInsert);
			}
			//if (settings.dimInactiveRegions && params.fileVersion === openFileVersions.get(params.uri)) {
			// Apply the decorations to all *visible* text editors
			const editors: TextEditor[] = window.visibleTextEditors.filter(e => e.document.uri.toString() === params.uri);
			for (const e of editors) {
					e.setDecorations(decoration, ranges);
			}
			//}
	}

};




export function activate(context: ExtensionContext) {
	console.log("Greetings from pascal-language-server 🙏");

	// Load the path to the language server from settings
	let executable: string = workspace.getConfiguration('pascalLanguageServer').get("executable");

	// TODO: download the executable for the active platform
	// https://github.com/genericptr/pascal-language-server/releases/download/x86_64-darwin/pasls
	// if (!executable) {
	// 	let target = 'x86_64-darwin';
	// 	executable = context.asAbsolutePath(path.join('bin', target, 'pasls'));
	// }

	console.log("executable: " + executable);

	// load environment variables from settings which are used for CodeTools
	let userEnvironmentVariables = {};
	let keys: string[] = ['PP', 'FPCDIR', 'LAZARUSDIR', 'FPCTARGET', 'FPCTARGETCPU'];
	let settingEnvironmentVariables = workspace.getConfiguration('pascalLanguageServer.env');

	Object.keys(settingEnvironmentVariables).forEach(key => {
		if (keys.includes(key)) {
			if (settingEnvironmentVariables[key]) userEnvironmentVariables[key] = settingEnvironmentVariables[key];
		}
	});


	let transport: string = workspace.getConfiguration('pascalLanguageServer').get('transport') || 'stdio';
	transport = transport.toLowerCase();
	let tcpHost: string = workspace.getConfiguration('pascalLanguageServer').get('tcp.host') || '127.0.0.1';
	let tcpPort: number = workspace.getConfiguration('pascalLanguageServer').get('tcp.port') || 4002;

	let serverOptions: ServerOptions;

	if (transport === 'tcp') {
		serverOptions = () => {
			return new Promise((resolve, reject) => {
				let settled = false;

				const startTime = Date.now();
				const maxWaitMs = 10_000;
				const retryDelayMs = 200;

				const attemptConnect = () => {
					if (settled) return;

					let socket = net.connect({ host: tcpHost, port: tcpPort });
					tcpSocket = socket;
					socket.setNoDelay(true);

					socket.once('connect', () => {
						if (settled) return;
						settled = true;

						resolve({
							detached: false,
							reader: new StreamMessageReader(socket),
							writer: new StreamMessageWriter(socket)
						} as any);
					});

					socket.once('error', (err: Error) => {
						socket.destroy();
						tcpSocket = undefined;

						if (settled) return;
						if (Date.now() - startTime >= maxWaitMs) {
							settled = true;
							reject(err);
							return;
						}
						setTimeout(attemptConnect, retryDelayMs);
					});
				};

				attemptConnect();
			});
		};
	} else {
		let run: Executable = {
			command: executable,
			options: {
				env: userEnvironmentVariables
			}
		};
		let debug: Executable = run;
		serverOptions = {
			run: run,
			debug: debug
		};
	}

	let initializationOptions = workspace.getConfiguration('pascalLanguageServer.initializationOptions');

	// console.log(initializationOptions);

	// client extensions configure their server
	let clientOptions: LanguageClientOptions = {
		initializationOptions: initializationOptions,
		// workspaceFolder: folder,
		documentSelector: [
			{ scheme: 'file', language: 'pascal' },
			{ scheme: 'untitled', language: 'pascal' }
		]
	}

	client = new LanguageClient('pascalLanguageServer', 'Pascal Language Server', serverOptions, clientOptions);
	client.start();
	client.onReady().then(function() {
		client.onNotification(InactiveRegionNotification,setInactiveRegion);
  });

	/* Completion command  */
	const completecmd = commands.registerCommand(InvokeCompleteCommand, (document, range) => {
		let activeEditor = window.activeTextEditor;
		let curPos = activeEditor.selection.active;
		let doc = document ? document : activeEditor.document;
		let rng = range ? range : new Range(curPos, curPos);
		if (doc.uri && rng) {
			commands.executeCommand(CompleteCommand, doc.uri.with({ "scheme": "file" }).toString(), rng.start);
		}
	});

	context.subscriptions.push(completecmd);

        /* Register formatting provider registration*/

	languages.registerDocumentFormattingEditProvider('pascal', {
		provideDocumentFormattingEdits(document: TextDocument): TextEdit[] {
			invokeFormat(document,new Range(new Position(0,0), new Position(document.lineCount,0)));
			return [];
		}
	});	

	/* Format Code  command  */
	
	const formatcmd = commands.registerCommand(InvokeFormatCommand, invokeFormat)

	context.subscriptions.push(formatcmd);

	/* Invert assignment command  */

	const invertassignmentcmd = commands.registerCommand(InvokeInvertAssignmentCommand, invokeInvertAssignment)

	context.subscriptions.push(invertassignmentcmd);

       /* Remove empty methods command */

	const removeemptymethodscmd = commands.registerCommand(InvokeRemoveEmptyMethodsCommand, invokeRemoveEmptyMethods)

	context.subscriptions.push(removeemptymethodscmd);
	

}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		if (tcpSocket) {
			tcpSocket.destroy();
			tcpSocket = undefined;
		}
		return undefined;
	}
	return client.stop().then(() => {
		if (tcpSocket) {
			tcpSocket.destroy();
			tcpSocket = undefined;
		}
	});
}
